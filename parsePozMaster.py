#!/usr/bin/env python

import os, sys
import pylab
import numpy as np
import scipy
import scipy.cluster
import datetime

from matplotlib import pyplot as plt
from matplotlib.collections import LineCollection

from sklearn import manifold
from sklearn.metrics import euclidean_distances
from sklearn.decomposition import PCA

########################
def addPoz(data, minRank=0, maxRank=50):

    onesToAdd = ['Al Kaline'
                 ,'Nap Lajoie'
                 ,'Bob Feller'
                 ,'Albert Pujols'
                 ,'Sandy Koufax'
                 ,'Yogi Berra'
                 ,'Pedro Martinez'
                 ,'Warren Spahn'
                 ,'Jackie Robinson'
                 ,'Pete Rose'
                 ,'Eddie Collins'

                 ]

    countArray = range(maxRank, minRank-1, -1)
    itr = onesToAdd
    k = 'Poz'

    data[k] = {}    

    itr = zip(range(50, 50-len(onesToAdd), -1), onesToAdd)

    for i, v in enumerate(itr):
        irank, name = v[:]
        if irank>=minRank and irank<=maxRank:
            data[k][name] = irank

    return len(data[k])

########################
def getData(ifile='Poz100-Master_v08.csv', minRank=0, maxRank=50, vbose=0):
    # data has contestant as key
    data = {}
    # nn has prediction as key
    nn = {}

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
                if not rank in nn:
                    nn[rank] = {}
                continue
            if i>0 and (not (rank>=minRank and rank<=maxRank)):
                if vbose>0:
                    print 'at rank=', rank, 'continue'
                continue
            pl = s
            con = cons[i]
            if not con in data:
                data[con] = {}
            data[con][pl] = rank

            if not pl in nn[rank]:
                nn[rank][pl] = 0
            nn[rank][pl] += 1

    return data, nn

########################
def computeSimSc(data, con1, con2, vbose=0, minRank=0, maxRank=50, NPOZ=50):
    x = []

    l1 = data[con1]
    l2 = data[con2]

    if vbose:
        print '*********************'
        print 'con', con1, len(l1), con2, len(l2)

    for name in l1:
        s1 = l1[name]
        if vbose:
            print name, s1, 
        if len(l2)<NPOZ and 'Poz' in con2:
            sc = 0
        elif name in l2:
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

        if s1>=minRank and s1<=maxRank:
            x.append(sc)
        else:
            if vbose>=1:
                print 'skip', s1, 'not between', minRank, 'and', maxRank
    if vbose:
        print sum(x)
    return pylab.array(x)

########################
def getDissim(data, atype, vbose=0, minRank=0, maxRank=50, NPOZ=50):

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

            ss1 = computeSimSc(data, k_con, k_pl, vbose=vbose, minRank=minRank, maxRank=maxRank, NPOZ=NPOZ)
            ss2 = computeSimSc(data, k_pl, k_con, vbose=vbose, minRank=minRank, maxRank=maxRank, NPOZ=NPOZ)

            
            if atype=='abs':
                sc1 = sum(ss1)
                sc2 = sum(ss2)
            elif atype=='rms':
                sc1 = pylab.sqrt(pylab.sum(ss1**2))
                sc2 = pylab.sqrt(pylab.sum(ss2**2))
            elif atype=='met':
                sc1 = sum(pylab.logical_and(ss1!=0, True)) 
                sc2 = sum(pylab.logical_and(ss2!=0, True))

            if vbose>=1:
                print 'score for ', k_con, k_pl, ss1, sc1, ss2, sc2

            oldsc = sc1 + sc2
            oldsc *= 0.5


            l1 = len(data[k_con])
            l2 = len(data[k_pl])
            iscale = min(l1, l2)
            nsc = oldsc/(1.0*iscale)
            if vbose>=1:
                print k_con, k_pl, 'oldsc', oldsc, l1, l2, iscale, 'nsc', nsc

            
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

        ivl = np.array(ivl, dtype='S10')
        ivw = len(ivl) * 10

        ivlpl = ivl[:]
            
        ivticks = np.arange(5, len(ivlpl)*10+5, 10)
        pylab.xticks(ivticks, ivlpl, rotation=+33, size='small')

        axis = pylab.gca()
        lbls=axis.get_xticklabels()

#    scipy.cluster.hierarchy.dendrogram(Z, color_threshold=0,truncate_mode='level', p=10, show_contracted=True)



########################
def countOccurances(data):
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
def rotateData(names, pos, name='Poz', name2='fWAR', vbose=0):

    flipX = flipY = +1

    for i, v in enumerate(names):
        if v==name:
            xpos, ypos = pos[i][:]
            theta = pylab.arctan2(xpos, ypos)


    for i, v in enumerate(names):
        xpos, ypos = pos[i][:]

        nxpos = xpos*pylab.cos(theta) - ypos*pylab.sin(theta)
        nypos = xpos*pylab.sin(theta) + ypos*pylab.cos(theta)        

        pos[i][0] = nxpos
        pos[i][1] = nypos

        if vbose>=1:
            print v, 'oldpos', xpos, ypos, 'newpos(rota)', nxpos, nypos

    for i, v in enumerate(names):
        if v==name2:
            x2, y2 = pos[i][:]
            if x2<0:
                flipX = -1
            if y2>0:
                flixY = -1

    for i, v in enumerate(names):
        xpos, ypos = pos[i][:]
        nxpos = xpos*flipX
        nypos = ypos*flipY
        pos[i][0] = nxpos
        pos[i][1] = nypos
        if vbose>=1:
            print v, 'oldpos', xpos, ypos, 'newpos(flip)', nxpos, nypos

    return 

########################
def doMds(data, atype, minRank, maxRank, vbose=0, NPOZ=50, ishow=False):
    seed = np.random.RandomState(seed=3)
    names, dissim, matr = getDissim(data, atype, vbose=vbose, minRank=minRank, maxRank=maxRank, NPOZ=NPOZ)
    print matr

    mds = manifold.MDS(n_components=2, max_iter=3000, eps=1e-9, random_state=seed, dissimilarity="precomputed", n_jobs=1)
    pos = mds.fit(matr).embedding_
    print pos
    pylab.clf()

    rotateData(names, pos, vbose=vbose)
    for i, n in enumerate(names):
        if 'Poz' in n:
            pylab.text(pos[i][0], pos[i][1], n, family='monospace', size='small', color='r')
        else:
            pylab.text(pos[i][0], pos[i][1], n, family='monospace', size='x-small')

    xmin, xmax = min(pos[:,0]), max(pos[:,0])
    ymin, ymax = min(pos[:,1]), max(pos[:,1])
    dx = 0.05*(xmax-xmin)
    dy = 0.05*(ymax-ymin)
    pylab.xlim(xmin-dx, xmax+dx)
    pylab.ylim(ymin-dy, ymax+dy)

    if ishow:
        pylab.show()

    return zip(names, pos)
 
########################
def doCsvLoop(ifile, atype, minRank, maxRank, vbose=0, ofile='poz100_mds.csv'):

    ofp = open(ofile, 'w')
    ofp.write('minRank,name,xpos,ypos\n')
    for r in range(maxRank, minRank-1, -1):
        minR = r
        maxR = maxRank
        data, nn = getData(ifile, minRank=minR, maxRank=maxR)
        NPOZ = addPoz(data, minRank=minR, maxRank=maxR)
        ans = doMds(data, atype, minRank, maxRank, vbose=vbose, NPOZ=NPOZ, ishow=False)

        scale = 0.0
        sortme = []
        for i, v in enumerate(ans):
            name = v[0]
            xpos, ypos = v[1][:]
            rad = pylab.sqrt(xpos**2+ypos**2)
            if rad>scale:
                scale = rad
            sortme.append([name, xpos, ypos])

        sortme.sort()
        for i, v in enumerate(sortme):
            name = v[0]
            xpos, ypos = v[1], v[2]
            ofp.write('%d,%s,%.4f,%.4f\n' % (r, name, xpos/scale, ypos/scale))
    ofp.close()

########################
def doPercentiles(data, pozFile):
    import mlb
    m = mlb.mlb()
    pp = {}
    for c in data.keys():
        g = data[c]
        for k in g.keys():
            if not k in pp:
                pp[k] = []
            pp[k].append(g[k])

    poz = m.csvToArray('pozAnswers.csv', ikeys=['rank'], skeys=['name'])

    tot  = len(pp['Babe Ruth']) # everyone picked babe ruth!
    tot = 1.0*tot

    for d in poz:
        r = d['rank']
        k = d['name']
        pk = np.array(pp[k])

        f = (1.0*len(pk))/tot
        print '%5.2f %5.2f %5.2f %5.2f %s ' % (f, 100*sum(pk<r)/tot, 100*sum(pk==r)/tot, 100*sum(pk>=r)/tot, k)
#        plt.clf()
#        plt.hist(np.array(pk)-r, bins=41)
#        plt.show()

########################
if __name__=='__main__':

    atype = 'rms'

    ifile = 'Poz100-Master_v08.csv'
    vbose = 0
    n2do = 10
    cmethod = 'single'
    ishow = True
    cmetric = 'euclidean'
    minRank = 0
    maxRank = 50

    minRank = 40
    maxRank = 50

    iCsvLoop = False

    for ia, a in enumerate(sys.argv):
        if a=='-vbose':
            vbose = int(sys.argv[ia+1])
        if a=='-atype':
            atype = sys.argv[ia+1]
        if a=='-minRank':
            minRank = int(sys.argv[ia+1])
        if a=='-maxRank':
            maxRank = int(sys.argv[ia+1])
        if a=='-csvLoop':
            iCsvLoop = bool(int(sys.argv[ia+1]))

    if iCsvLoop:
        doCsvLoop(ifile, atype, minRank, maxRank, vbose=vbose)
        sys.exit()

    data, nn = getData(ifile, minRank=minRank, maxRank=maxRank)

    NPOZ = addPoz(data, minRank=minRank, maxRank=maxRank)

    ans = doMds(data, atype, minRank, maxRank, vbose=vbose, NPOZ=NPOZ, ishow=True)

    ofile = 'poz100_%d.csv' % minRank
    ofp = open(ofile, 'w')
    ofp.write('name,xpos,ypos\n')

    for i, v in enumerate(ans):
        name = v[0]
        xpos, ypos = v[1][:]
        if name=='fivetwentyone':
            theta = pylab.arctan2(xpos, ypos)
            
    for i, v in enumerate(ans):
        name = v[0]
        xpos, ypos = v[1][:]
        if name=='Poz' and minRank<42:
            xpos = -1.0
            ypos = +1.0
        ofp.write('%s,%.4f,%.4f\n' % (name, xpos, ypos))
    ofp.close()

#    doDendro(names, dissim, vbose=0,cmetric = 'euclidean')

    if ishow:
        pylab.show()    
