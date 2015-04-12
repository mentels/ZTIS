#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pyquery import PyQuery as pq
from xml.etree.ElementTree import fromstring

base_url = 'http://stat.gov.pl/statystyka-regionalna/rankingi-statystyczne/%s'

def population_province():
    path = 'ludnosc-wedlug-wojewodztw'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        all = int(row[2].text.strip())
        men = int(row[3].text.strip())
        women = int(row[4].text.strip())
        assert all == men + women
        print '%s %d %d %d ' % (province, all, men, women)

def area_province():
    path = 'powierzchnia-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        area = int(row[2].text.strip())
        print '%s %d' % (province, area)

def income_per_inhabitant_province():
    path = 'dochody-budzetow-na-1-mieszkanca-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        income = float(row[2].text.strip().replace(',', '.'))
        print '%s %.2f' % (province, income)


def outcome_per_inhabitant_province():
    path = 'wydatki-budzetow-na-1-mieszkanca-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        outcome = float(row[2].text.strip().replace(',', '.'))
        print '%s %.2f' % (province, outcome)


def birthrate_for_1000_inhabitants_province():
    path = 'przyrost-naturalny-na-1000-ludnosci-wedlug-wojewodztw/'
    page = pq(url=base_url % path)
    for row in page('div.desc-module').find('tr')[2:]:
        province = row[1].text.strip()
        birthrate = float(row[2].text.strip().replace(',', '.'))
        print '%s %.2f' % (province, birthrate)

def run():
    print '===> Ludnosc'
    population_province()
    print '\n===> Powierzchnia'
    area_province()
    print '\n===> Dochody na mieszkanca'
    income_per_inhabitant_province()
    print '\n===> Wydatki na mieszkanca'
    outcome_per_inhabitant_province()
    print '\n===> Przyrost naturalny na 1000 ludności'
    birthrate_for_1000_inhabitants_province()


if __name__ == '__main__':
    run()

