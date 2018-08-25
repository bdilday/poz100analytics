#!/usr/bin/env python

import matplotlib
import numpy as np
import parsePozMaster as ppm
from matplotlib import pyplot as plt

###########################
if __name__=='__main__':
    data = ppm.getData(ifile='Poz100-Master_v08.csv')
    zz, nb, aa = ppm.coutOccurances(data)
    
    ks = aa.keys()
    ks.sort()
    ks = ks[::-1]
    i2name = {}
    name2i = {}
    nPlayers = len(nb)
    nranks = 50
    vbose = 0

    ylabs = []


    orderByAvgRank = True

    avgRanks = []
    for d in nb:
        name = d[1]
        aRank = d[2]
        avgRanks.append([aRank, name])

    avgRanks.sort()

    iname = 0
    if orderByAvgRank:
        for ar in avgRanks:
            jname = iname
            name = ar[1]
            i2name[jname] = name
            name2i[name] = jname
            iname += 1
            ylabs.append(name)
    else:
        for rKey in ks:
            dataWithinRank = aa[rKey]

            for v in dataWithinRank:
                jname = iname
                name = v[1]
                print iname, jname, name
                i2name[jname] = name
                name2i[name] = jname
                iname += 1
                ylabs.append(name)

    xlabs = range(1, 1+nranks)

    xx = []
    yy = []
    ntot = {}



    for z in zz:
        nentries, name, irank = z[:]
        if not name in ntot:
            ntot[name] = 0
            print name, irank, nentries
        for i in range(nentries):
            xx.append(name2i[name])
            yy.append(irank)
            ntot[name] += 1



    ans = plt.hist2d(xx, yy, bins=[range(nPlayers), range(1,nranks+2)])
    plt.clf()
    fig, ax = plt.subplots()
    plt.pcolor(ans[0], cmap=plt.cm.Blues_r)
    ax.set_xticks(np.arange(nranks)+0.5, minor=False)
    ax.set_yticks(np.arange(nPlayers)+0.5, minor=False)
    ax.xaxis.tick_top()
    ax.invert_yaxis()

    ax.set_xticklabels(xlabs, minor=False, size='xx-small')
    ax.set_yticklabels(ylabs, minor=False, size='xx-small')
    plt.ylim(60, 0)
    plt.xlim(0, 50)
    
    plt.show()
    



