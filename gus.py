#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pyquery import PyQuery as pq
from xml.etree.ElementTree import fromstring
import xlrd

base_url = 'http://stat.gov.pl/statystyka-regionalna/rankingi-statystyczne/%s'
provinces_file = 'provinces.txt'
bezrobotni_xls = 'bezrobotni_stopa_wg_powiatow_10_2014.xls'
bezrobotni_xls_description = 'bezrobotni_stopa_wg_powiatow_10_2014_description.txt'

def load_provinces():
    with open(provinces_file, 'r') as f:
        provinces_data = {}
        for line in f:
            provinces_data[unicode(line.strip(), encoding='utf8')] = []
    return provinces_data

def population_province(data):
    path = 'ludnosc-wedlug-wojewodztw'
    page = pq(url=base_url % path)
    headers = ['populacja', 'populacja: mężczyźni', 'populacja: kobiety']
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        all = int(row[2].text.strip())
        men = int(row[3].text.strip())
        women = int(row[4].text.strip())
        assert all == men + women
        data[province].extend([all, men, women])
    return headers

def area_province():
    path = 'powierzchnia-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        area = int(row[2].text.strip())
        print '%s %d' % (province, area)

def income_per_inhabitant_province(data):
    path = 'dochody-budzetow-na-1-mieszkanca-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        income = float(row[2].text.strip().replace(',', '.'))
        data[province].append(income)
    return ['dochody budżetów na 1 mieszkańca']


def outcome_per_inhabitant_province(data):
    path = 'wydatki-budzetow-na-1-mieszkanca-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        outcome = float(row[2].text.strip().replace(',', '.'))
        data[province].append(outcome)
    return ['wydatki budżetów na 1 mieszkańca']


def birthrate_for_1000_inhabitants_province(data):
    path = 'przyrost-naturalny-na-1000-ludnosci-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        birthrate = float(row[2].text.strip().replace(',', '.'))
        data[province].append(birthrate)
    return ['przyrost naturlany na 1000 ludności']

def write_output(headers, data):
    with open('output.data', 'w') as f:
        f.write(','.join(headers) + '\n')
        for province in data:
            line = province
            for prop in data[province]:
                ## TODO: float?
                if isinstance(prop, float):
                    line += ',%.2f' % prop
                else:
                    line += ',%s' % prop
            f.write(line.encode('utf8') + '\n')

def read_unemployment_rate_from_xls(data):
    unemployment_rate_col = 0
    with open(bezrobotni_xls_description, 'r') as f:
        xls_description = {}
        for i, line in enumerate(f):
            if i == 16:
                # 65 is an ASCII code for A
                unemployment_rate_col = ord(line.strip()) - 65
            elif i == 17:
                county_code_col = ord(line.strip()) - 65
            else:
                province = line.split()[0]
                sheet_row = line.split()[1]
                county_cnt = line.split()[2]
                # -1 because numering starts from 0 in xlrd
                province_as_key = unicode(province.strip(), encoding='utf8')
                xls_description[province_as_key] = [int(sheet_row) - 1, int(county_cnt)]
    xls = xlrd.open_workbook(bezrobotni_xls)
    sheet = xls.sheet_by_name('10_14')
    for province in xls_description:
        province_row = xls_description[province][0]
        county_cnt = xls_description[province][1]
        unemployment_rate =  sheet.cell_value(province_row, unemployment_rate_col)
        avg = calculate_avg_unemployment_by_county(sheet,
                                                   province,
                                                   xls_description,
                                                   county_code_col,
                                                   unemployment_rate_col)
        data[province].append(unemployment_rate)
        data[province].append(avg)
    return ['Stopa bezrobocia; koniec X 2014', 'Średnia z powiatów']

def calculate_avg_unemployment_by_county(sheet, province, xls_description, 
                                         county_code_col, unemployment_rate_col):
    province_row = xls_description[province][0]
    county_cnt = xls_description[province][1]
    county_row = 1
    sum = 0
    while county_cnt:
        if not sheet.cell_value(province_row + county_row, county_code_col) == '00':
            county_cnt -= 1
            sum += sheet.cell_value(province_row + county_row, unemployment_rate_col)
        county_row += 1
    return sum / xls_description[province][1]

def run():
    data =  load_provinces()
    headers = ['województwo']
    headers.extend(population_province(data))
    # print '\n===> Powierzchnia'
    # area_province()
    headers.extend(income_per_inhabitant_province(data))
    headers.extend(outcome_per_inhabitant_province(data))
    headers.extend(birthrate_for_1000_inhabitants_province(data))
    headers.extend(read_unemployment_rate_from_xls(data))
    write_output(headers, data)


if __name__ == '__main__':
    run()


