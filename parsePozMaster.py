#!/usr/bin/env python

import os, sys
import pylab
import numpy
import scipy
import scipy.cluster
import datetime

from matplotlib import pyplot as plt
from matplotlib.collections import LineCollection

from sklearn import manifold
from sklearn.metrics import euclidean_distances
from sklearn.decomposition import PCA

########################
def getData(ifile='Poz100-Master_v08.csv'):
    data = {}

    lines = [l.strip() for l in open(ifile).readlines()]

    hd = lines[0]
    st = hd.split(',')
    cons = {}
    for i, k in enumerate(st[:]):
        cons[i] = k

    for il, l in enumerate(lines[0:51]):
        if il==0:
            continue
        if len(l)<=1:
            continue
        st = l.split(',')
        for i, s in enumerate(st[0:]):
            if i==0:
                rank = int(s)
                continue
            pl = s
            con = cons[i]
            if not con in data:
                data[con] = {}
            data[con][pl] = rank

    return data

########################
def computeSimSc(data, con1, con2, vbose=0):
    x = []
    if vbose:
        print '*********************'
        print 'con', con1, con2
    l1 = data[con1]
    l2 = data[con2]
    for name in l1:
        s1 = l1[name]
        if vbose:
            print name, s1, 
        if name in l2:
            s2 = l2[name]
            sc = abs(s2-s1)
            if vbose:
                print s2,
        else:
            sc = 50
            if vbose:
                print '--',
        if vbose:
            print sc
        x.append(sc)
    if vbose:
        print sum(x)
    return pylab.array(x)

########################
def getDissim(data, atype, vbose=0):
    ks = data.keys()

    matr = pylab.ones(len(ks)**2)
    matr = pylab.reshape(matr, (len(ks), len(ks)))
    scs = []
    names = []
    for ik, k_con in enumerate(ks):
        name = ik
        if not k_con in names:
            names.append(k_con)
        for jk, k_pl in enumerate(ks):

            ss = computeSimSc(data, k_con, k_pl, vbose=vbose)
            
            if atype=='abs':
                nsc = sum(ss)
            elif atype=='rms':
                nsc = pylab.sqrt(pylab.mean(ss**2))
            elif atype=='met':
                nsc = sum(pylab.logical_and(ss!=0, True))

            matr[ik][jk] = nsc
            if jk<=ik:
                continue

            print nsc, 'xx', ik, k_con, jk, k_pl
            scs.append(nsc)
    return names, pylab.array(scs), matr

########################
def doDendro(names, dissim, vbose=0,cmetric = 'euclidean'):

    Y = pylab.array(dissim)
    Z = scipy.cluster.hierarchy.linkage(Y,method=cmethod,metric=cmetric)
    blah = scipy.cluster.hierarchy.dendrogram(Z)

#    pylab.hist(dissim, bins=100)
#    pylab.show()

    ids = names[:]

    if not ids is None:
        ivlpl = []
        ivl = []
        ivl_str = blah['ivl']
        for i, s in enumerate(ivl_str):
            j = int(s)
            thisid = ids[j]
            thisid = thisid.split()[-1]
            ivl.append(thisid)

        ivl = numpy.array(ivl, dtype='S10')
        ivw = len(ivl) * 10

        ivlpl = ivl[:]
            
        ivticks = numpy.arange(5, len(ivlpl)*10+5, 10)
        pylab.xticks(ivticks, ivlpl, rotation=+33, size='small')

        axis = pylab.gca()
        lbls=axis.get_xticklabels()

#    scipy.cluster.hierarchy.dendrogram(Z, color_threshold=0,truncate_mode='level', p=10, show_contracted=True)



########################
def coutOccurances(data):
    ss = {}
    nb = {}
    sumRank = {}
    for contestant, v in data.items():
        for playerName, irank in v.items():
            if not playerName in ss:
                ss[playerName] = {}
                nb[playerName] = 0
                sumRank[playerName] = 0
            if not irank in ss[playerName]:
                ss[playerName][irank] = 0
            ss[playerName][irank] += 1            
            nb[playerName] += 1
            sumRank[playerName] += irank


    xx = []
    nballots = []
    for k in ss:
        for r in ss[k]:
            xx.append([ss[k][r], k, r])

    for k in nb:
        nballots.append([nb[k], k, sumRank[k]/(1.0*nb[k])])

    aa = {}
    for d in nballots:
        nb = d[0]
        if not nb in aa:
            aa[nb] = []
        aa[nb].append([d[2], d[1]])

    for k in aa:
        aa[k].sort() 

    xx.sort()
    nballots.sort()
    return xx, nballots, aa
    
########################
if __name__=='__main__':

    atype = 'rms'
    seed = numpy.random.RandomState(seed=3)

    ifile = 'Poz100-Master_v08.csv'
    vbose = 0
    n2do = 10
    cmethod = 'single'
    ishow = True
    cmetric = 'euclidean'

    for ia, a in enumerate(sys.argv):
        if a=='-vbose':
            vbose = int(sys.argv[ia+1])
        if a=='-atype':
            atype = sys.argv[ia+1]

    data = getData(ifile)
    names, dissim, matr = getDissim(data, atype, vbose=vbose)
    print matr

    mds = manifold.MDS(n_components=2, max_iter=3000, eps=1e-9, random_state=seed, dissimilarity="precomputed", n_jobs=1)
    pos = mds.fit(matr).embedding_
    print pos
    pylab.clf()
    for i, n in enumerate(names):
        pylab.text(pos[i][0], pos[i][1], n, family='monospace', size='x-small')

    xmin, xmax = min(pos[:,0]), max(pos[:,0])
    ymin, ymax = min(pos[:,1]), max(pos[:,1])
    dx = 0.05*(xmax-xmin)
    dy = 0.05*(ymax-ymin)
    pylab.xlim(xmin-dx, xmax+dx)
    pylab.ylim(ymin-dy, ymax+dy)
    pylab.show()
    

#    doDendro(names, dissim, vbose=0,cmetric = 'euclidean')

    if ishow:
        pylab.show()    
