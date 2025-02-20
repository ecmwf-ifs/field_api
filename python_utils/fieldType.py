# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

class fieldType (object):
  def __init__ (self, **kwargs):
    self.__dict__ = kwargs
    self.suffix = self.kind[2:]

    tt = self.suffix[0]
    ss = self.suffix[1]

    th = {'R': 'REAL', 'I': 'INTEGER', 'L': 'LOGICAL'}
    td = {'R':  '0.0', 'I':       '0', 'L': '.FALSE.'}

    self.type = th[tt] + '(KIND=' + self.kind + ')'
    self.default = td[tt] + '_' + self.kind

    self.name = 'FIELD_%s%s' % (self.rank, self.suffix);
    self.rank = int (self.rank)
    self.shape = ','.join ([':'] * int (self.rank))
    self.viewRank = self.rank-1
    self.viewShape = ','.join ([':'] * (self.rank-1))
    self.lbptr = ', '.join (list (map (lambda i: "LBOUNDS(" + str (i+1) + "):", range (0, self.rank))))
    self.lbptr_blk = ', '.join([ f"LBOUNDS({i}):" for i in range(1, self.rank)] + ["BLK_BOUNDS(1):"])
    self.hst_blk = ':, ' * (self.rank-1) + 'BLK_BOUNDS(1):BLK_BOUNDS(2)'
    self.devptr_blk = ':, ' * (self.rank-1) + f'LBOUNDS({self.rank}) + LOCAL_OFFSET:LBOUNDS({self.rank}) + LOCAL_OFFSET + BLK_BOUNDS(2)-BLK_BOUNDS(1)'
    self.hasView = self.rank > 1
    self.ganged = self.rank > 2

    h5th = {'R': 'REAL', 'I': 'INTEGER', 'L': 'INTEGER'}
    self.h5type ='H5T_NATIVE_' + h5th[tt]
    self.h5kind ='H5_' + h5th[tt] + '_KIND'

kinds = ['JPRM', 'JPRD', 'JPIM', 'JPLM']

def getFieldTypeList (ranks=[1,2,3,4,5], kinds=kinds, hasView=None, ganged=None):
  
  l = [fieldType (kind=kind, rank=rank) for (kind) in kinds for rank in ranks]

  if hasView != None:
    l = [ft for ft in l if ft.hasView == hasView]
  
  if ganged != None:
    l = [ft for ft in l if ft.ganged == ganged]
  
  return l

def useParkind1 (kinds=kinds):
  return 'USE PARKIND1, ONLY : ' + ', '.join (kinds)
