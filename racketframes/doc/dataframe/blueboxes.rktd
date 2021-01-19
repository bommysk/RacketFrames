33
((3) 0 () 0 () () (h ! (equal)))
procedure
(new-ISeries data label) -> ISeries?
  data : (Vectorof Fixnum)
  label : (Option (U (Listof Label) SIndex))
procedure
(iseries-iref iseries idx) -> Fixnum?
  iseries : ISeries
  idx : Index
procedure
(iseries-label-ref iseries label) -> Fixnum?
  iseries : ISeries
  label : Label
procedure
(iseries-range iseries pos) -> (Vectorof Fixnum)
  iseries : ISeries
  pos : Index
procedure
(iseries-length iseries) -> Index
  iseries : ISeries
procedure
(iseries-referencer iseries) -> (Index -> Fixnum)
  iseries : ISeries
procedure
(iseries-data iseries) -> (Vectorof Fixnum)
  iseries : ISeries
procedure
(map/is iseries fn) -> ISeries
  iseries : ISeries
  fn : (Fixnum -> Fixnum)
procedure
(bop/is iseries iseries-2 fn) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
  fn : (Fixnum Fixnum -> Fixnum)
procedure
(comp/is iseries iseries-2 fn) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
  fn : (Fixnum Fixnum -> Boolean)
procedure
(+/is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(+/is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(*/is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(//is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(%/is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(r/is iseries iseries-2) -> ISeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(+./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(-./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(*./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(/./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(%./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(r./is iseries num) -> ISeries
  iseries : ISeries
  num : Fixnum
procedure
(>/is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(</is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(>=/is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(<=/is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(=/is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(!=/is iseries iseries-2) -> BSeries
  iseries : ISeries
  iseries-2 : ISeries
procedure
(apply-agg-is func iseries) -> Real
  func : Symbol
  iseries : ISeries
procedure
(apply-stat-is func iseries) -> Real
  func : Symbol
  iseries : ISeries
procedure
(new-NSeries data label) -> NSeries?
  data : (Vectorof Float)
  label : (Option (U (Lnstof Label) SIndex))
procedure
(nseries-iref nseries idx) -> Float?
  nseries : NSeries
  idx : Index
procedure
(nseries-label-ref nseries label) -> Float?
  nseries : NSeries
  label : Label
procedure
(nseries-range nseries pos) -> (Vectorof Float)
  nseries : NSeries
  pos : Index
procedure
(nseries-length nseries) -> Index
  nseries : NSeries
procedure
(nseries-referencer nseries) -> (Index -> Float)
  nseries : NSeries
procedure
(nseries-data nseries) -> (Vectorof Float)
  nseries : NSeries
procedure
(map/ns nseries fn) -> NSeries
  nseries : NSeries
  fn : (Float -> Float)
procedure
(bop/ns nseries nseries-2 fn) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
  fn : (Float Float -> Float)
procedure
(comp/ns nseries nseries-2 fn) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
  fn : (Float Float -> Boolean)
procedure
(+/ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(+/ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(*/ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(//ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(%/ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(r/ns nseries nseries-2) -> NSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(+./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(-./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(*./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(/./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(%./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(r./ns nseries num) -> NSeries
  nseries : NSeries
  num : Float
procedure
(>/ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(</ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(>=/ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(<=/ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(=/ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(!=/ns nseries nseries-2) -> BSeries
  nseries : NSeries
  nseries-2 : NSeries
procedure
(apply-agg-ns func nseries) -> Real
  func : Symbol
  nseries : NSeries
procedure
(apply-stat-ns func nseries) -> Real
  func : Symbol
  nseries : NSeries
procedure
(new-data-frame columns) -> DataFrame?
  columns : Columns
procedure
(data-frame-rename df col new-col) -> DataFrame?
  df : DataFrame
  col : Label
  new-col : Label
procedure
(data-frame-drop df col-name) -> DataFrame?
  df : DataFrame
  col-name : Label
procedure
(data-frame-drop df col-name) -> Series?
  df : DataFrame
  col-name : Label
procedure
(data-frame-names df col-name) -> Series?
  df : DataFrame
  col-name : Label
procedure
(data-frame-dim df) -> Dim?
  df : DataFrame
procedure
(data-frame-description df project) -> DataFrameDescription?
  df : DataFrame
  project : LabelProjection
procedure
(show-data-frame-description dfd) -> Void?
  dfd : DataFrameDescription
procedure
(data-frame-explode df project) -> Columns?
  df : DataFrame
  project : LabelProjection
procedure
(data-frame-remove df project) -> DataFrame?
  df : DataFrame
  project : LabelProjection
procedure
(data-frame-project df project) -> DataFrame?
  df : DataFrame
  project : LabelProjection
procedure
(data-frame-replace df col) -> DataFrame?
  df : DataFrame
  col : Column
procedure
(data-frame-extend df col) -> DataFrame?
  df : DataFrame
  col : (U Column Columns DataFrame)
procedure
(data-frame+ dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame- dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame* dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame/ dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame% dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame-r dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame= dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame!= dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame< dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame> dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame<= dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame>= dfa dfb) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
procedure
(data-frame-abs df) -> DataFrame?
  df : DataFrame
procedure
(data-frame-join-left dfa dfb on) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  on : (Listof Symbol)
procedure
(data-frame-join-right dfa dfb on) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  on : (Listof Symbol)
procedure
(data-frame-join-inner dfa dfb on) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  on : (Listof Symbol)
procedure
(data-frame-join-outer dfa dfb on) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  on : (Listof Symbol)
procedure
(data-frame-concat-vertical dfa dfb col) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  col : (Listof Symbol)
procedure
(data-frame-concat-horizontal dfa dfb col) -> DataFrame?
  dfa : DataFrame
  dfb : DataFrame
  col : (Listof Symbol)
procedure
(data-frame-groupby dfa by) -> GroupHash?
  dfa : DataFrame
  by : (Listof Symbol)
procedure
(apply-agg-data-frame function-name     
                      group-hash)   -> AggValueHash?
  function-name : Symbol
  group-hash : GroupHash
