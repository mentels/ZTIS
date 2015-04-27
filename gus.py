#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pyquery import PyQuery as pq
from xml.etree.ElementTree import fromstring

base_url = 'http://stat.gov.pl/statystyka-regionalna/rankingi-statystyczne/%s'
provinces_file = 'provinces.txt'

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
                line += ',%s' % prop
            f.write(line.encode('utf8') + '\n')

def run():
    data =  load_provinces()
    headers = ['województwo']
    headers.extend(population_province(data))
    # print '\n===> Powierzchnia'
    # area_province()
    headers.extend(income_per_inhabitant_province(data))
    headers.extend(outcome_per_inhabitant_province(data))
    headers.extend(birthrate_for_1000_inhabitants_province(data))
    write_output(headers, data)


if __name__ == '__main__':
    run()


