#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pyquery import PyQuery as pq
import collections


def extract_table_header(table):
    cols_desc = collections.defaultdict(lambda: "")
    rows_spans = collections.defaultdict(lambda: 0)
    for tr in table.find('tr'):
        if tr.find('th') is not None:
            i = 0
            for cell in tr:
                while rows_spans[i] > 1:
                    rows_spans[i] -= 1
                    i += 1
                colspans = int(cell.attrib.get('colspan', 1))
                rowspans = int(cell.attrib.get('rowspan', 1))
                for j in range(0, colspans):
                    rows_spans[i] = rowspans
                    pcell = pq(cell)
                    if pcell.html() is not None and pcell.html().strip() != "":
                        cols_desc[i] += " " + pcell.html().strip().replace('<br/>', '').replace('\n', ' ')
                        cols_desc[i] = cols_desc[i].strip()
                    i += 1
        else:
            break
    return [item[1] for item in sorted(cols_desc.items(), key=lambda i: i[0])]


def extract_table_data(table):
    rows = list()
    for tr in table.find('tr'):
        if tr.find('td') is not None:
            row = list()
            for i, cell in enumerate(tr):
                row.append(pq(cell).html())
            rows.append(row)
    return rows


def map_table(table, f):
    for i, r in enumerate(table):
        for j, c in enumerate(r):
            table[i][j] = f(i, j, c)


def map_col(table, col, f):
    def _f(i, j, c):
        if j == col:
            return f(c)
        else:
            return c

    map_table(table, _f)


def get_cols(l, cols):
    return [val for i, val in enumerate(l) if i in cols]


def print_col(header, data, cols):
    print " | ".join(get_cols(header, cols))
    for r in data:
        print ", ".join(get_cols(r, cols))


def wiki_province_table():
    table = pq(url="http://pl.wikipedia.org/wiki/Podzia%C5%82_administracyjny_Polski").find('table.wikitable')
    header = extract_table_header(table)
    data = extract_table_data(table)
    map_col(data, 1, lambda c: pq(c).find('a').text())
    map_col(data, 2, lambda c: pq(c).text())

    def remove_span(c):
        h = pq(c)
        h.find('span').remove()
        return h.html()

    map_col(data, 3, remove_span)
    data = data[:-2]
    for col in range(2, len(header)):
        print_col(header, data, (1, col))
        print


if __name__ == '__main__':
    wiki_province_table()