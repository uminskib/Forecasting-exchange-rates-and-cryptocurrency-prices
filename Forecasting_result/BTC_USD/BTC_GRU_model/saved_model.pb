ф©$
љ°
D
AddV2
x"T
y"T
z"T"
Ttype:
2	АР
B
AssignVariableOp
resource
value"dtype"
dtypetypeИ
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(И
?
Mul
x"T
y"T
z"T"
Ttype:
2	Р

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetypeИ
E
Relu
features"T
activations"T"
Ttype:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0И
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0И
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
[
Split
	split_dim

value"T
output"T*	num_split"
	num_splitint(0"	
Ttype
Л
SplitV

value"T
size_splits"Tlen
	split_dim
output"T*	num_split"
	num_splitint(0"	
Ttype"
Tlentype0	:
2	
Њ
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring И
@
StaticRegexFullMatch	
input

output
"
patternstring
ц
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
<
Sub
x"T
y"T
z"T"
Ttype:
2	
Ђ
TensorListFromTensor
tensor"element_dtype
element_shape"
shape_type*
output_handleКйelement_dtype"
element_dtypetype"

shape_typetype:
2	
Ъ
TensorListReserve
element_shape"
shape_type
num_elements#
handleКйelement_dtype"
element_dtypetype"

shape_typetype:
2	
И
TensorListStack
input_handle
element_shape
tensor"element_dtype"
element_dtypetype" 
num_elementsint€€€€€€€€€
P
	Transpose
x"T
perm"Tperm
y"T"	
Ttype"
Tpermtype0:
2	
P
Unpack

value"T
output"T*num"
numint("	
Ttype"
axisint 
Ц
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 И
Ф
While

input2T
output2T"
T
list(type)("
condfunc"
bodyfunc" 
output_shapeslist(shape)
 "
parallel_iterationsint
И"serve*2.6.02unknown8юл"
t
dense/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namedense/kernel
m
 dense/kernel/Read/ReadVariableOpReadVariableOpdense/kernel*
_output_shapes

: *
dtype0
l

dense/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_name
dense/bias
e
dense/bias/Read/ReadVariableOpReadVariableOp
dense/bias*
_output_shapes
:*
dtype0
Г
gru/gru_cell/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*$
shared_namegru/gru_cell/kernel
|
'gru/gru_cell/kernel/Read/ReadVariableOpReadVariableOpgru/gru_cell/kernel*
_output_shapes
:	а*
dtype0
Ш
gru/gru_cell/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
†а*.
shared_namegru/gru_cell/recurrent_kernel
С
1gru/gru_cell/recurrent_kernel/Read/ReadVariableOpReadVariableOpgru/gru_cell/recurrent_kernel* 
_output_shapes
:
†а*
dtype0

gru/gru_cell/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*"
shared_namegru/gru_cell/bias
x
%gru/gru_cell/bias/Read/ReadVariableOpReadVariableOpgru/gru_cell/bias*
_output_shapes
:	а*
dtype0
Л
gru_1/gru_cell_1/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	†`*(
shared_namegru_1/gru_cell_1/kernel
Д
+gru_1/gru_cell_1/kernel/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/kernel*
_output_shapes
:	†`*
dtype0
Ю
!gru_1/gru_cell_1/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*2
shared_name#!gru_1/gru_cell_1/recurrent_kernel
Ч
5gru_1/gru_cell_1/recurrent_kernel/Read/ReadVariableOpReadVariableOp!gru_1/gru_cell_1/recurrent_kernel*
_output_shapes

: `*
dtype0
Ж
gru_1/gru_cell_1/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*&
shared_namegru_1/gru_cell_1/bias

)gru_1/gru_cell_1/bias/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/bias*
_output_shapes

:`*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
x
dense/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namedense/kernel/m
q
"dense/kernel/m/Read/ReadVariableOpReadVariableOpdense/kernel/m*
_output_shapes

: *
dtype0
p
dense/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense/bias/m
i
 dense/bias/m/Read/ReadVariableOpReadVariableOpdense/bias/m*
_output_shapes
:*
dtype0
З
gru/gru_cell/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*&
shared_namegru/gru_cell/kernel/m
А
)gru/gru_cell/kernel/m/Read/ReadVariableOpReadVariableOpgru/gru_cell/kernel/m*
_output_shapes
:	а*
dtype0
Ь
gru/gru_cell/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
†а*0
shared_name!gru/gru_cell/recurrent_kernel/m
Х
3gru/gru_cell/recurrent_kernel/m/Read/ReadVariableOpReadVariableOpgru/gru_cell/recurrent_kernel/m* 
_output_shapes
:
†а*
dtype0
Г
gru/gru_cell/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*$
shared_namegru/gru_cell/bias/m
|
'gru/gru_cell/bias/m/Read/ReadVariableOpReadVariableOpgru/gru_cell/bias/m*
_output_shapes
:	а*
dtype0
П
gru_1/gru_cell_1/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	†`**
shared_namegru_1/gru_cell_1/kernel/m
И
-gru_1/gru_cell_1/kernel/m/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/kernel/m*
_output_shapes
:	†`*
dtype0
Ґ
#gru_1/gru_cell_1/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*4
shared_name%#gru_1/gru_cell_1/recurrent_kernel/m
Ы
7gru_1/gru_cell_1/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp#gru_1/gru_cell_1/recurrent_kernel/m*
_output_shapes

: `*
dtype0
К
gru_1/gru_cell_1/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*(
shared_namegru_1/gru_cell_1/bias/m
Г
+gru_1/gru_cell_1/bias/m/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/bias/m*
_output_shapes

:`*
dtype0
x
dense/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namedense/kernel/v
q
"dense/kernel/v/Read/ReadVariableOpReadVariableOpdense/kernel/v*
_output_shapes

: *
dtype0
p
dense/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense/bias/v
i
 dense/bias/v/Read/ReadVariableOpReadVariableOpdense/bias/v*
_output_shapes
:*
dtype0
З
gru/gru_cell/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*&
shared_namegru/gru_cell/kernel/v
А
)gru/gru_cell/kernel/v/Read/ReadVariableOpReadVariableOpgru/gru_cell/kernel/v*
_output_shapes
:	а*
dtype0
Ь
gru/gru_cell/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
†а*0
shared_name!gru/gru_cell/recurrent_kernel/v
Х
3gru/gru_cell/recurrent_kernel/v/Read/ReadVariableOpReadVariableOpgru/gru_cell/recurrent_kernel/v* 
_output_shapes
:
†а*
dtype0
Г
gru/gru_cell/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	а*$
shared_namegru/gru_cell/bias/v
|
'gru/gru_cell/bias/v/Read/ReadVariableOpReadVariableOpgru/gru_cell/bias/v*
_output_shapes
:	а*
dtype0
П
gru_1/gru_cell_1/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	†`**
shared_namegru_1/gru_cell_1/kernel/v
И
-gru_1/gru_cell_1/kernel/v/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/kernel/v*
_output_shapes
:	†`*
dtype0
Ґ
#gru_1/gru_cell_1/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*4
shared_name%#gru_1/gru_cell_1/recurrent_kernel/v
Ы
7gru_1/gru_cell_1/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp#gru_1/gru_cell_1/recurrent_kernel/v*
_output_shapes

: `*
dtype0
К
gru_1/gru_cell_1/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*(
shared_namegru_1/gru_cell_1/bias/v
Г
+gru_1/gru_cell_1/bias/v/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/bias/v*
_output_shapes

:`*
dtype0

NoOpNoOp
В0
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*љ/
value≥/B∞/ B©/
у
layer_with_weights-0
layer-0
layer-1
layer_with_weights-1
layer-2
layer_with_weights-2
layer-3
	optimizer
trainable_variables
	variables
regularization_losses
		keras_api


signatures
l
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
R
trainable_variables
	variables
regularization_losses
	keras_api
l
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
h

kernel
bias
trainable_variables
	variables
regularization_losses
 	keras_api
Рm_m`!ma"mb#mc$md%me&mfvgvh!vi"vj#vk$vl%vm&vn
8
!0
"1
#2
$3
%4
&5
6
7
8
!0
"1
#2
$3
%4
&5
6
7
 
≠

'layers
(layer_metrics
trainable_variables
)metrics
*non_trainable_variables
	variables
+layer_regularization_losses
regularization_losses
 
~

!kernel
"recurrent_kernel
#bias
,trainable_variables
-	variables
.regularization_losses
/	keras_api
 

!0
"1
#2

!0
"1
#2
 
є

0layers
1layer_metrics
trainable_variables
2metrics
3non_trainable_variables
	variables
4layer_regularization_losses

5states
regularization_losses
 
 
 
≠

6layers
7layer_metrics
trainable_variables
8metrics
9non_trainable_variables
	variables
:layer_regularization_losses
regularization_losses
~

$kernel
%recurrent_kernel
&bias
;trainable_variables
<	variables
=regularization_losses
>	keras_api
 

$0
%1
&2

$0
%1
&2
 
є

?layers
@layer_metrics
trainable_variables
Ametrics
Bnon_trainable_variables
	variables
Clayer_regularization_losses

Dstates
regularization_losses
XV
VARIABLE_VALUEdense/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
TR
VARIABLE_VALUE
dense/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
≠

Elayers
Flayer_metrics
trainable_variables
Gmetrics
Hnon_trainable_variables
	variables
Ilayer_regularization_losses
regularization_losses
YW
VARIABLE_VALUEgru/gru_cell/kernel0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUE
ca
VARIABLE_VALUEgru/gru_cell/recurrent_kernel0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEgru/gru_cell/bias0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUEgru_1/gru_cell_1/kernel0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUE
ge
VARIABLE_VALUE!gru_1/gru_cell_1/recurrent_kernel0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUE
[Y
VARIABLE_VALUEgru_1/gru_cell_1/bias0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUE

0
1
2
3
 

J0
K1
 
 

!0
"1
#2

!0
"1
#2
 
≠

Llayers
Mlayer_metrics
,trainable_variables
Nmetrics
Onon_trainable_variables
-	variables
Player_regularization_losses
.regularization_losses

0
 
 
 
 
 
 
 
 
 
 

$0
%1
&2

$0
%1
&2
 
≠

Qlayers
Rlayer_metrics
;trainable_variables
Smetrics
Tnon_trainable_variables
<	variables
Ulayer_regularization_losses
=regularization_losses

0
 
 
 
 
 
 
 
 
 
 
4
	Vtotal
	Wcount
X	variables
Y	keras_api
D
	Ztotal
	[count
\
_fn_kwargs
]	variables
^	keras_api
 
 
 
 
 
 
 
 
 
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

V0
W1

X	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

Z0
[1

]	variables
vt
VARIABLE_VALUEdense/kernel/mRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
rp
VARIABLE_VALUEdense/bias/mPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
wu
VARIABLE_VALUEgru/gru_cell/kernel/mLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
Б
VARIABLE_VALUEgru/gru_cell/recurrent_kernel/mLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
us
VARIABLE_VALUEgru/gru_cell/bias/mLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEgru_1/gru_cell_1/kernel/mLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
ЖГ
VARIABLE_VALUE#gru_1/gru_cell_1/recurrent_kernel/mLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
yw
VARIABLE_VALUEgru_1/gru_cell_1/bias/mLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEdense/kernel/vRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
rp
VARIABLE_VALUEdense/bias/vPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
wu
VARIABLE_VALUEgru/gru_cell/kernel/vLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
Б
VARIABLE_VALUEgru/gru_cell/recurrent_kernel/vLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
us
VARIABLE_VALUEgru/gru_cell/bias/vLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEgru_1/gru_cell_1/kernel/vLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
ЖГ
VARIABLE_VALUE#gru_1/gru_cell_1/recurrent_kernel/vLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
yw
VARIABLE_VALUEgru_1/gru_cell_1/bias/vLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
Д
serving_default_gru_inputPlaceholder*+
_output_shapes
:€€€€€€€€€*
dtype0* 
shape:€€€€€€€€€
ъ
StatefulPartitionedCallStatefulPartitionedCallserving_default_gru_inputgru/gru_cell/biasgru/gru_cell/kernelgru/gru_cell/recurrent_kernelgru_1/gru_cell_1/biasgru_1/gru_cell_1/kernel!gru_1/gru_cell_1/recurrent_kerneldense/kernel
dense/bias*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В */
f*R(
&__inference_signature_wrapper_25920652
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
≈
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename dense/kernel/Read/ReadVariableOpdense/bias/Read/ReadVariableOp'gru/gru_cell/kernel/Read/ReadVariableOp1gru/gru_cell/recurrent_kernel/Read/ReadVariableOp%gru/gru_cell/bias/Read/ReadVariableOp+gru_1/gru_cell_1/kernel/Read/ReadVariableOp5gru_1/gru_cell_1/recurrent_kernel/Read/ReadVariableOp)gru_1/gru_cell_1/bias/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOp"dense/kernel/m/Read/ReadVariableOp dense/bias/m/Read/ReadVariableOp)gru/gru_cell/kernel/m/Read/ReadVariableOp3gru/gru_cell/recurrent_kernel/m/Read/ReadVariableOp'gru/gru_cell/bias/m/Read/ReadVariableOp-gru_1/gru_cell_1/kernel/m/Read/ReadVariableOp7gru_1/gru_cell_1/recurrent_kernel/m/Read/ReadVariableOp+gru_1/gru_cell_1/bias/m/Read/ReadVariableOp"dense/kernel/v/Read/ReadVariableOp dense/bias/v/Read/ReadVariableOp)gru/gru_cell/kernel/v/Read/ReadVariableOp3gru/gru_cell/recurrent_kernel/v/Read/ReadVariableOp'gru/gru_cell/bias/v/Read/ReadVariableOp-gru_1/gru_cell_1/kernel/v/Read/ReadVariableOp7gru_1/gru_cell_1/recurrent_kernel/v/Read/ReadVariableOp+gru_1/gru_cell_1/bias/v/Read/ReadVariableOpConst*)
Tin"
 2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В **
f%R#
!__inference__traced_save_25922996
Р
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense/kernel
dense/biasgru/gru_cell/kernelgru/gru_cell/recurrent_kernelgru/gru_cell/biasgru_1/gru_cell_1/kernel!gru_1/gru_cell_1/recurrent_kernelgru_1/gru_cell_1/biastotalcounttotal_1count_1dense/kernel/mdense/bias/mgru/gru_cell/kernel/mgru/gru_cell/recurrent_kernel/mgru/gru_cell/bias/mgru_1/gru_cell_1/kernel/m#gru_1/gru_cell_1/recurrent_kernel/mgru_1/gru_cell_1/bias/mdense/kernel/vdense/bias/vgru/gru_cell/kernel/vgru/gru_cell/recurrent_kernel/vgru/gru_cell/bias/vgru_1/gru_cell_1/kernel/v#gru_1/gru_cell_1/recurrent_kernel/vgru_1/gru_cell_1/bias/v*(
Tin!
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *-
f(R&
$__inference__traced_restore_25923090рк!
ВC
с
while_body_25921536
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
а
d
E__inference_dropout_layer_call_and_return_conditional_losses_25920316

inputs
identityИc
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *д8О?2
dropout/Constx
dropout/MulMulinputsdropout/Const:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/MulT
dropout/ShapeShapeinputs*
T0*
_output_shapes
:2
dropout/Shape≈
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*,
_output_shapes
:€€€€€€€€€†*
dtype0*

seed02&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ=2
dropout/GreaterEqual/y√
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/GreaterEqualД
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:€€€€€€€€€†2
dropout/Cast
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/Mul_1j
IdentityIdentitydropout/Mul_1:z:0*
T0*,
_output_shapes
:€€€€€€€€€†2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
Д
і
while_cond_25922218
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25922218___redundant_placeholder06
2while_while_cond_25922218___redundant_placeholder16
2while_while_cond_25922218___redundant_placeholder26
2while_while_cond_25922218___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
Ж
c
E__inference_dropout_layer_call_and_return_conditional_losses_25921980

inputs

identity_1_
IdentityIdentityinputs*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityn

Identity_1IdentityIdentity:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2

Identity_1"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
Ж
і
while_cond_25921688
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25921688___redundant_placeholder06
2while_while_cond_25921688___redundant_placeholder16
2while_while_cond_25921688___redundant_placeholder26
2while_while_cond_25921688___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
≥	
¶
gru_1_while_cond_25920865(
$gru_1_while_gru_1_while_loop_counter.
*gru_1_while_gru_1_while_maximum_iterations
gru_1_while_placeholder
gru_1_while_placeholder_1
gru_1_while_placeholder_2*
&gru_1_while_less_gru_1_strided_slice_1B
>gru_1_while_gru_1_while_cond_25920865___redundant_placeholder0B
>gru_1_while_gru_1_while_cond_25920865___redundant_placeholder1B
>gru_1_while_gru_1_while_cond_25920865___redundant_placeholder2B
>gru_1_while_gru_1_while_cond_25920865___redundant_placeholder3
gru_1_while_identity
О
gru_1/while/LessLessgru_1_while_placeholder&gru_1_while_less_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
gru_1/while/Lesso
gru_1/while/IdentityIdentitygru_1/while/Less:z:0*
T0
*
_output_shapes
: 2
gru_1/while/Identity"5
gru_1_while_identitygru_1/while/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
Ч

–
-__inference_sequential_layer_call_fn_25920111
	gru_input
unknown:	а
	unknown_0:	а
	unknown_1:
†а
	unknown_2:`
	unknown_3:	†`
	unknown_4: `
	unknown_5: 
	unknown_6:
identityИҐStatefulPartitionedCall…
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_sequential_layer_call_and_return_conditional_losses_259200922
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
“
F
*__inference_dropout_layer_call_fn_25921997

inputs
identity»
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259199132
PartitionedCallq
IdentityIdentityPartitionedCall:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
ЭI
й
gru_while_body_25920716$
 gru_while_gru_while_loop_counter*
&gru_while_gru_while_maximum_iterations
gru_while_placeholder
gru_while_placeholder_1
gru_while_placeholder_2#
gru_while_gru_strided_slice_1_0_
[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0?
,gru_while_gru_cell_readvariableop_resource_0:	аF
3gru_while_gru_cell_matmul_readvariableop_resource_0:	аI
5gru_while_gru_cell_matmul_1_readvariableop_resource_0:
†а
gru_while_identity
gru_while_identity_1
gru_while_identity_2
gru_while_identity_3
gru_while_identity_4!
gru_while_gru_strided_slice_1]
Ygru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor=
*gru_while_gru_cell_readvariableop_resource:	аD
1gru_while_gru_cell_matmul_readvariableop_resource:	аG
3gru_while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ(gru/while/gru_cell/MatMul/ReadVariableOpҐ*gru/while/gru_cell/MatMul_1/ReadVariableOpҐ!gru/while/gru_cell/ReadVariableOpЋ
;gru/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2=
;gru/while/TensorArrayV2Read/TensorListGetItem/element_shapeл
-gru/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0gru_while_placeholderDgru/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02/
-gru/while/TensorArrayV2Read/TensorListGetItemі
!gru/while/gru_cell/ReadVariableOpReadVariableOp,gru_while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02#
!gru/while/gru_cell/ReadVariableOp•
gru/while/gru_cell/unstackUnpack)gru/while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru/while/gru_cell/unstack…
(gru/while/gru_cell/MatMul/ReadVariableOpReadVariableOp3gru_while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02*
(gru/while/gru_cell/MatMul/ReadVariableOpџ
gru/while/gru_cell/MatMulMatMul4gru/while/TensorArrayV2Read/TensorListGetItem:item:00gru/while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/MatMulј
gru/while/gru_cell/BiasAddBiasAdd#gru/while/gru_cell/MatMul:product:0#gru/while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/BiasAddУ
"gru/while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"gru/while/gru_cell/split/split_dimы
gru/while/gru_cell/splitSplit+gru/while/gru_cell/split/split_dim:output:0#gru/while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/while/gru_cell/split–
*gru/while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp5gru_while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02,
*gru/while/gru_cell/MatMul_1/ReadVariableOpƒ
gru/while/gru_cell/MatMul_1MatMulgru_while_placeholder_22gru/while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/MatMul_1∆
gru/while/gru_cell/BiasAdd_1BiasAdd%gru/while/gru_cell/MatMul_1:product:0#gru/while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/BiasAdd_1Й
gru/while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru/while/gru_cell/ConstЧ
$gru/while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2&
$gru/while/gru_cell/split_1/split_dim≥
gru/while/gru_cell/split_1SplitV%gru/while/gru_cell/BiasAdd_1:output:0!gru/while/gru_cell/Const:output:0-gru/while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/while/gru_cell/split_1і
gru/while/gru_cell/addAddV2!gru/while/gru_cell/split:output:0#gru/while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/addТ
gru/while/gru_cell/SigmoidSigmoidgru/while/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/SigmoidЄ
gru/while/gru_cell/add_1AddV2!gru/while/gru_cell/split:output:1#gru/while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_1Ш
gru/while/gru_cell/Sigmoid_1Sigmoidgru/while/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/Sigmoid_1±
gru/while/gru_cell/mulMul gru/while/gru_cell/Sigmoid_1:y:0#gru/while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mulѓ
gru/while/gru_cell/add_2AddV2!gru/while/gru_cell/split:output:2gru/while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_2Л
gru/while/gru_cell/ReluRelugru/while/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/ReluІ
gru/while/gru_cell/mul_1Mulgru/while/gru_cell/Sigmoid:y:0gru_while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mul_1y
gru/while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru/while/gru_cell/sub/x≠
gru/while/gru_cell/subSub!gru/while/gru_cell/sub/x:output:0gru/while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/sub±
gru/while/gru_cell/mul_2Mulgru/while/gru_cell/sub:z:0%gru/while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mul_2ђ
gru/while/gru_cell/add_3AddV2gru/while/gru_cell/mul_1:z:0gru/while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_3р
.gru/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemgru_while_placeholder_1gru_while_placeholdergru/while/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype020
.gru/while/TensorArrayV2Write/TensorListSetItemd
gru/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru/while/add/yy
gru/while/addAddV2gru_while_placeholdergru/while/add/y:output:0*
T0*
_output_shapes
: 2
gru/while/addh
gru/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru/while/add_1/yК
gru/while/add_1AddV2 gru_while_gru_while_loop_countergru/while/add_1/y:output:0*
T0*
_output_shapes
: 2
gru/while/add_1{
gru/while/IdentityIdentitygru/while/add_1:z:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/IdentityТ
gru/while/Identity_1Identity&gru_while_gru_while_maximum_iterations^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_1}
gru/while/Identity_2Identitygru/while/add:z:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_2™
gru/while/Identity_3Identity>gru/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_3Ъ
gru/while/Identity_4Identitygru/while/gru_cell/add_3:z:0^gru/while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/Identity_4ё
gru/while/NoOpNoOp)^gru/while/gru_cell/MatMul/ReadVariableOp+^gru/while/gru_cell/MatMul_1/ReadVariableOp"^gru/while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
gru/while/NoOp"l
3gru_while_gru_cell_matmul_1_readvariableop_resource5gru_while_gru_cell_matmul_1_readvariableop_resource_0"h
1gru_while_gru_cell_matmul_readvariableop_resource3gru_while_gru_cell_matmul_readvariableop_resource_0"Z
*gru_while_gru_cell_readvariableop_resource,gru_while_gru_cell_readvariableop_resource_0"@
gru_while_gru_strided_slice_1gru_while_gru_strided_slice_1_0"1
gru_while_identitygru/while/Identity:output:0"5
gru_while_identity_1gru/while/Identity_1:output:0"5
gru_while_identity_2gru/while/Identity_2:output:0"5
gru_while_identity_3gru/while/Identity_3:output:0"5
gru_while_identity_4gru/while/Identity_4:output:0"Є
Ygru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2T
(gru/while/gru_cell/MatMul/ReadVariableOp(gru/while/gru_cell/MatMul/ReadVariableOp2X
*gru/while/gru_cell/MatMul_1/ReadVariableOp*gru/while/gru_cell/MatMul_1/ReadVariableOp2F
!gru/while/gru_cell/ReadVariableOp!gru/while/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
О

Ќ
-__inference_sequential_layer_call_fn_25921298

inputs
unknown:	а
	unknown_0:	а
	unknown_1:
†а
	unknown_2:`
	unknown_3:	†`
	unknown_4: `
	unknown_5: 
	unknown_6:
identityИҐStatefulPartitionedCall∆
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_sequential_layer_call_and_return_conditional_losses_259200922
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
≈V
ц
A__inference_gru_layer_call_and_return_conditional_losses_25919900

inputs3
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25919811*
condR
while_cond_25919810*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeй
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm¶
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeo
IdentityIdentitytranspose_1:y:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
ђX
Г
C__inference_gru_1_layer_call_and_return_conditional_losses_25922155
inputs_04
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЖ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25922066*
condR
while_cond_25922065*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:_ [
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
"
_user_specified_name
inputs/0
№
Ї
&__inference_gru_layer_call_fn_25921953
inputs_0
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identityИҐStatefulPartitionedCallО
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259189492
StatefulPartitionedCallЙ
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0
Ц
–
H__inference_sequential_layer_call_and_return_conditional_losses_25920092

inputs
gru_25919901:	а
gru_25919903:	а 
gru_25919905:
†а 
gru_1_25920068:`!
gru_1_25920070:	†` 
gru_1_25920072: ` 
dense_25920086: 
dense_25920088:
identityИҐdense/StatefulPartitionedCallҐgru/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallЦ
gru/StatefulPartitionedCallStatefulPartitionedCallinputsgru_25919901gru_25919903gru_25919905*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259199002
gru/StatefulPartitionedCallц
dropout/PartitionedCallPartitionedCall$gru/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259199132
dropout/PartitionedCallЈ
gru_1/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0gru_1_25920068gru_1_25920070gru_1_25920072*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259200672
gru_1/StatefulPartitionedCallЂ
dense/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_25920086dense_25920088*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_dense_layer_call_and_return_conditional_losses_259200852
dense/StatefulPartitionedCallБ
IdentityIdentity&dense/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityђ
NoOpNoOp^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
у
ў
F__inference_gru_cell_layer_call_and_return_conditional_losses_25918822

inputs

states*
readvariableop_resource:	а1
matmul_readvariableop_resource:	а4
 matmul_1_readvariableop_resource:
†а
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpy
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes
:	а*
dtype02
ReadVariableOpl
unstackUnpackReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	а*
dtype02
MatMul/ReadVariableOpt
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
MatMult
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimѓ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
splitХ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02
MatMul_1/ReadVariableOpz
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2

MatMul_1z
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim‘
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2	
split_1h
addAddV2split:output:0split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
addY
SigmoidSigmoidadd:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2	
Sigmoidl
add_1AddV2split:output:1split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
add_1_
	Sigmoid_1Sigmoid	add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	Sigmoid_1e
mulMulSigmoid_1:y:0split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
mulc
add_2AddV2split:output:2mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_2R
ReluRelu	add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
Relu]
mul_1MulSigmoid:y:0states*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xa
subSubsub/x:output:0Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sube
mul_2Mulsub:z:0Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_2`
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_3e
IdentityIdentity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identityi

Identity_1Identity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:PL
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_namestates
цW
Б
C__inference_gru_1_layer_call_and_return_conditional_losses_25922614

inputs4
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm{
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25922525*
condR
while_cond_25922524*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeи
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm•
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
§|
В
$__inference__traced_restore_25923090
file_prefix/
assignvariableop_dense_kernel: +
assignvariableop_1_dense_bias:9
&assignvariableop_2_gru_gru_cell_kernel:	аD
0assignvariableop_3_gru_gru_cell_recurrent_kernel:
†а7
$assignvariableop_4_gru_gru_cell_bias:	а=
*assignvariableop_5_gru_1_gru_cell_1_kernel:	†`F
4assignvariableop_6_gru_1_gru_cell_1_recurrent_kernel: `:
(assignvariableop_7_gru_1_gru_cell_1_bias:`"
assignvariableop_8_total: "
assignvariableop_9_count: %
assignvariableop_10_total_1: %
assignvariableop_11_count_1: 4
"assignvariableop_12_dense_kernel_m: .
 assignvariableop_13_dense_bias_m:<
)assignvariableop_14_gru_gru_cell_kernel_m:	аG
3assignvariableop_15_gru_gru_cell_recurrent_kernel_m:
†а:
'assignvariableop_16_gru_gru_cell_bias_m:	а@
-assignvariableop_17_gru_1_gru_cell_1_kernel_m:	†`I
7assignvariableop_18_gru_1_gru_cell_1_recurrent_kernel_m: `=
+assignvariableop_19_gru_1_gru_cell_1_bias_m:`4
"assignvariableop_20_dense_kernel_v: .
 assignvariableop_21_dense_bias_v:<
)assignvariableop_22_gru_gru_cell_kernel_v:	аG
3assignvariableop_23_gru_gru_cell_recurrent_kernel_v:
†а:
'assignvariableop_24_gru_gru_cell_bias_v:	а@
-assignvariableop_25_gru_1_gru_cell_1_kernel_v:	†`I
7assignvariableop_26_gru_1_gru_cell_1_recurrent_kernel_v: `=
+assignvariableop_27_gru_1_gru_cell_1_bias_v:`
identity_29ИҐAssignVariableOpҐAssignVariableOp_1ҐAssignVariableOp_10ҐAssignVariableOp_11ҐAssignVariableOp_12ҐAssignVariableOp_13ҐAssignVariableOp_14ҐAssignVariableOp_15ҐAssignVariableOp_16ҐAssignVariableOp_17ҐAssignVariableOp_18ҐAssignVariableOp_19ҐAssignVariableOp_2ҐAssignVariableOp_20ҐAssignVariableOp_21ҐAssignVariableOp_22ҐAssignVariableOp_23ҐAssignVariableOp_24ҐAssignVariableOp_25ҐAssignVariableOp_26ҐAssignVariableOp_27ҐAssignVariableOp_3ҐAssignVariableOp_4ҐAssignVariableOp_5ҐAssignVariableOp_6ҐAssignVariableOp_7ҐAssignVariableOp_8ҐAssignVariableOp_9Н
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*Щ
valueПBМB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names»
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*M
valueDBBB B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slicesљ
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*И
_output_shapesv
t:::::::::::::::::::::::::::::*+
dtypes!
22
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:2

IdentityЬ
AssignVariableOpAssignVariableOpassignvariableop_dense_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1Ґ
AssignVariableOp_1AssignVariableOpassignvariableop_1_dense_biasIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2Ђ
AssignVariableOp_2AssignVariableOp&assignvariableop_2_gru_gru_cell_kernelIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3µ
AssignVariableOp_3AssignVariableOp0assignvariableop_3_gru_gru_cell_recurrent_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4©
AssignVariableOp_4AssignVariableOp$assignvariableop_4_gru_gru_cell_biasIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5ѓ
AssignVariableOp_5AssignVariableOp*assignvariableop_5_gru_1_gru_cell_1_kernelIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6є
AssignVariableOp_6AssignVariableOp4assignvariableop_6_gru_1_gru_cell_1_recurrent_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7≠
AssignVariableOp_7AssignVariableOp(assignvariableop_7_gru_1_gru_cell_1_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8Э
AssignVariableOp_8AssignVariableOpassignvariableop_8_totalIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9Э
AssignVariableOp_9AssignVariableOpassignvariableop_9_countIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10£
AssignVariableOp_10AssignVariableOpassignvariableop_10_total_1Identity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11£
AssignVariableOp_11AssignVariableOpassignvariableop_11_count_1Identity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12™
AssignVariableOp_12AssignVariableOp"assignvariableop_12_dense_kernel_mIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13®
AssignVariableOp_13AssignVariableOp assignvariableop_13_dense_bias_mIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14±
AssignVariableOp_14AssignVariableOp)assignvariableop_14_gru_gru_cell_kernel_mIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15ї
AssignVariableOp_15AssignVariableOp3assignvariableop_15_gru_gru_cell_recurrent_kernel_mIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_15n
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:2
Identity_16ѓ
AssignVariableOp_16AssignVariableOp'assignvariableop_16_gru_gru_cell_bias_mIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_16n
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:2
Identity_17µ
AssignVariableOp_17AssignVariableOp-assignvariableop_17_gru_1_gru_cell_1_kernel_mIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_17n
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:2
Identity_18њ
AssignVariableOp_18AssignVariableOp7assignvariableop_18_gru_1_gru_cell_1_recurrent_kernel_mIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_18n
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:2
Identity_19≥
AssignVariableOp_19AssignVariableOp+assignvariableop_19_gru_1_gru_cell_1_bias_mIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_19n
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:2
Identity_20™
AssignVariableOp_20AssignVariableOp"assignvariableop_20_dense_kernel_vIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_20n
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:2
Identity_21®
AssignVariableOp_21AssignVariableOp assignvariableop_21_dense_bias_vIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_21n
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:2
Identity_22±
AssignVariableOp_22AssignVariableOp)assignvariableop_22_gru_gru_cell_kernel_vIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_22n
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:2
Identity_23ї
AssignVariableOp_23AssignVariableOp3assignvariableop_23_gru_gru_cell_recurrent_kernel_vIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_23n
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:2
Identity_24ѓ
AssignVariableOp_24AssignVariableOp'assignvariableop_24_gru_gru_cell_bias_vIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_24n
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:2
Identity_25µ
AssignVariableOp_25AssignVariableOp-assignvariableop_25_gru_1_gru_cell_1_kernel_vIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_25n
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:2
Identity_26њ
AssignVariableOp_26AssignVariableOp7assignvariableop_26_gru_1_gru_cell_1_recurrent_kernel_vIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_26n
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0*
_output_shapes
:2
Identity_27≥
AssignVariableOp_27AssignVariableOp+assignvariableop_27_gru_1_gru_cell_1_bias_vIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_279
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp∆
Identity_28Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_28f
Identity_29IdentityIdentity_28:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_29Ѓ
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 2
NoOp_1"#
identity_29Identity_29:output:0*M
_input_shapes<
:: : : : : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232*
AssignVariableOp_24AssignVariableOp_242*
AssignVariableOp_25AssignVariableOp_252*
AssignVariableOp_26AssignVariableOp_262*
AssignVariableOp_27AssignVariableOp_272(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
у

Џ
-__inference_gru_cell_1_layer_call_fn_25922875

inputs
states_0
unknown:`
	unknown_0:	†`
	unknown_1: `
identity

identity_1ИҐStatefulPartitionedCall§
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259192452
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0
їD
э
while_body_25922219
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
К;
ю
A__inference_gru_layer_call_and_return_conditional_losses_25918756

inputs$
gru_cell_25918680:	а$
gru_cell_25918682:	а%
gru_cell_25918684:
†а
identityИҐ gru_cell/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2и
 gru_cell/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_25918680gru_cell_25918682gru_cell_25918684*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259186792"
 gru_cell/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЗ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_25918680gru_cell_25918682gru_cell_25918684*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25918692*
condR
while_cond_25918691*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeт
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permѓ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimex
IdentityIdentitytranspose_1:y:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityy
NoOpNoOp!^gru_cell/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs
ё
Џ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922822

inputs
states_0)
readvariableop_resource:`1
matmul_readvariableop_resource:	†`2
 matmul_1_readvariableop_resource: `
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOpj
unstackUnpackReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
MatMuls
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimђ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
splitУ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2

MatMul_1y
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim—
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addX
SigmoidSigmoidadd:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
Sigmoidk
add_1AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_1^
	Sigmoid_1Sigmoid	add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	Sigmoid_1d
mulMulSigmoid_1:y:0split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
mulb
add_2AddV2split:output:2mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2Q
ReluRelu	add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Relu^
mul_1MulSigmoid:y:0states_0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/x`
subSubsub/x:output:0Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
subd
mul_2Mulsub:z:0Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_2_
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_3d
IdentityIdentity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh

Identity_1Identity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0
л	
…
&__inference_signature_wrapper_25920652
	gru_input
unknown:	а
	unknown_0:	а
	unknown_1:
†а
	unknown_2:`
	unknown_3:	†`
	unknown_4: `
	unknown_5: 
	unknown_6:
identityИҐStatefulPartitionedCall§
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В *,
f'R%
#__inference__wrapped_model_259186092
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
≈V
ц
A__inference_gru_layer_call_and_return_conditional_losses_25920485

inputs3
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25920396*
condR
while_cond_25920395*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeй
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm¶
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeo
IdentityIdentitytranspose_1:y:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Д
і
while_cond_25920197
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25920197___redundant_placeholder06
2while_while_cond_25920197___redundant_placeholder16
2while_while_cond_25920197___redundant_placeholder26
2while_while_cond_25920197___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
к^
ы
$sequential_gru_1_while_body_25918514>
:sequential_gru_1_while_sequential_gru_1_while_loop_counterD
@sequential_gru_1_while_sequential_gru_1_while_maximum_iterations&
"sequential_gru_1_while_placeholder(
$sequential_gru_1_while_placeholder_1(
$sequential_gru_1_while_placeholder_2=
9sequential_gru_1_while_sequential_gru_1_strided_slice_1_0y
usequential_gru_1_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_1_tensorarrayunstack_tensorlistfromtensor_0M
;sequential_gru_1_while_gru_cell_1_readvariableop_resource_0:`U
Bsequential_gru_1_while_gru_cell_1_matmul_readvariableop_resource_0:	†`V
Dsequential_gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0: `#
sequential_gru_1_while_identity%
!sequential_gru_1_while_identity_1%
!sequential_gru_1_while_identity_2%
!sequential_gru_1_while_identity_3%
!sequential_gru_1_while_identity_4;
7sequential_gru_1_while_sequential_gru_1_strided_slice_1w
ssequential_gru_1_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_1_tensorarrayunstack_tensorlistfromtensorK
9sequential_gru_1_while_gru_cell_1_readvariableop_resource:`S
@sequential_gru_1_while_gru_cell_1_matmul_readvariableop_resource:	†`T
Bsequential_gru_1_while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ7sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOpҐ9sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpҐ0sequential/gru_1/while/gru_cell_1/ReadVariableOpе
Hsequential/gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2J
Hsequential/gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeЇ
:sequential/gru_1/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemusequential_gru_1_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_1_tensorarrayunstack_tensorlistfromtensor_0"sequential_gru_1_while_placeholderQsequential/gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02<
:sequential/gru_1/while/TensorArrayV2Read/TensorListGetItemа
0sequential/gru_1/while/gru_cell_1/ReadVariableOpReadVariableOp;sequential_gru_1_while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype022
0sequential/gru_1/while/gru_cell_1/ReadVariableOp–
)sequential/gru_1/while/gru_cell_1/unstackUnpack8sequential/gru_1/while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2+
)sequential/gru_1/while/gru_cell_1/unstackц
7sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOpReadVariableOpBsequential_gru_1_while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype029
7sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOpФ
(sequential/gru_1/while/gru_cell_1/MatMulMatMulAsequential/gru_1/while/TensorArrayV2Read/TensorListGetItem:item:0?sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2*
(sequential/gru_1/while/gru_cell_1/MatMulы
)sequential/gru_1/while/gru_cell_1/BiasAddBiasAdd2sequential/gru_1/while/gru_cell_1/MatMul:product:02sequential/gru_1/while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2+
)sequential/gru_1/while/gru_cell_1/BiasAdd±
1sequential/gru_1/while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€23
1sequential/gru_1/while/gru_cell_1/split/split_dimі
'sequential/gru_1/while/gru_cell_1/splitSplit:sequential/gru_1/while/gru_cell_1/split/split_dim:output:02sequential/gru_1/while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2)
'sequential/gru_1/while/gru_cell_1/splitы
9sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOpDsequential_gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02;
9sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpэ
*sequential/gru_1/while/gru_cell_1/MatMul_1MatMul$sequential_gru_1_while_placeholder_2Asequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2,
*sequential/gru_1/while/gru_cell_1/MatMul_1Б
+sequential/gru_1/while/gru_cell_1/BiasAdd_1BiasAdd4sequential/gru_1/while/gru_cell_1/MatMul_1:product:02sequential/gru_1/while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2-
+sequential/gru_1/while/gru_cell_1/BiasAdd_1І
'sequential/gru_1/while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2)
'sequential/gru_1/while/gru_cell_1/Constµ
3sequential/gru_1/while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€25
3sequential/gru_1/while/gru_cell_1/split_1/split_dimы
)sequential/gru_1/while/gru_cell_1/split_1SplitV4sequential/gru_1/while/gru_cell_1/BiasAdd_1:output:00sequential/gru_1/while/gru_cell_1/Const:output:0<sequential/gru_1/while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2+
)sequential/gru_1/while/gru_cell_1/split_1п
%sequential/gru_1/while/gru_cell_1/addAddV20sequential/gru_1/while/gru_cell_1/split:output:02sequential/gru_1/while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2'
%sequential/gru_1/while/gru_cell_1/addЊ
)sequential/gru_1/while/gru_cell_1/SigmoidSigmoid)sequential/gru_1/while/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2+
)sequential/gru_1/while/gru_cell_1/Sigmoidу
'sequential/gru_1/while/gru_cell_1/add_1AddV20sequential/gru_1/while/gru_cell_1/split:output:12sequential/gru_1/while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential/gru_1/while/gru_cell_1/add_1ƒ
+sequential/gru_1/while/gru_cell_1/Sigmoid_1Sigmoid+sequential/gru_1/while/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2-
+sequential/gru_1/while/gru_cell_1/Sigmoid_1м
%sequential/gru_1/while/gru_cell_1/mulMul/sequential/gru_1/while/gru_cell_1/Sigmoid_1:y:02sequential/gru_1/while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2'
%sequential/gru_1/while/gru_cell_1/mulк
'sequential/gru_1/while/gru_cell_1/add_2AddV20sequential/gru_1/while/gru_cell_1/split:output:2)sequential/gru_1/while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential/gru_1/while/gru_cell_1/add_2Ј
&sequential/gru_1/while/gru_cell_1/ReluRelu+sequential/gru_1/while/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential/gru_1/while/gru_cell_1/Reluа
'sequential/gru_1/while/gru_cell_1/mul_1Mul-sequential/gru_1/while/gru_cell_1/Sigmoid:y:0$sequential_gru_1_while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential/gru_1/while/gru_cell_1/mul_1Ч
'sequential/gru_1/while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2)
'sequential/gru_1/while/gru_cell_1/sub/xи
%sequential/gru_1/while/gru_cell_1/subSub0sequential/gru_1/while/gru_cell_1/sub/x:output:0-sequential/gru_1/while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2'
%sequential/gru_1/while/gru_cell_1/subм
'sequential/gru_1/while/gru_cell_1/mul_2Mul)sequential/gru_1/while/gru_cell_1/sub:z:04sequential/gru_1/while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential/gru_1/while/gru_cell_1/mul_2з
'sequential/gru_1/while/gru_cell_1/add_3AddV2+sequential/gru_1/while/gru_cell_1/mul_1:z:0+sequential/gru_1/while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential/gru_1/while/gru_cell_1/add_3≥
;sequential/gru_1/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem$sequential_gru_1_while_placeholder_1"sequential_gru_1_while_placeholder+sequential/gru_1/while/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02=
;sequential/gru_1/while/TensorArrayV2Write/TensorListSetItem~
sequential/gru_1/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
sequential/gru_1/while/add/y≠
sequential/gru_1/while/addAddV2"sequential_gru_1_while_placeholder%sequential/gru_1/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential/gru_1/while/addВ
sequential/gru_1/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2 
sequential/gru_1/while/add_1/yЋ
sequential/gru_1/while/add_1AddV2:sequential_gru_1_while_sequential_gru_1_while_loop_counter'sequential/gru_1/while/add_1/y:output:0*
T0*
_output_shapes
: 2
sequential/gru_1/while/add_1ѓ
sequential/gru_1/while/IdentityIdentity sequential/gru_1/while/add_1:z:0^sequential/gru_1/while/NoOp*
T0*
_output_shapes
: 2!
sequential/gru_1/while/Identity”
!sequential/gru_1/while/Identity_1Identity@sequential_gru_1_while_sequential_gru_1_while_maximum_iterations^sequential/gru_1/while/NoOp*
T0*
_output_shapes
: 2#
!sequential/gru_1/while/Identity_1±
!sequential/gru_1/while/Identity_2Identitysequential/gru_1/while/add:z:0^sequential/gru_1/while/NoOp*
T0*
_output_shapes
: 2#
!sequential/gru_1/while/Identity_2ё
!sequential/gru_1/while/Identity_3IdentityKsequential/gru_1/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential/gru_1/while/NoOp*
T0*
_output_shapes
: 2#
!sequential/gru_1/while/Identity_3ѕ
!sequential/gru_1/while/Identity_4Identity+sequential/gru_1/while/gru_cell_1/add_3:z:0^sequential/gru_1/while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/while/Identity_4•
sequential/gru_1/while/NoOpNoOp8^sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOp:^sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp1^sequential/gru_1/while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
sequential/gru_1/while/NoOp"К
Bsequential_gru_1_while_gru_cell_1_matmul_1_readvariableop_resourceDsequential_gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0"Ж
@sequential_gru_1_while_gru_cell_1_matmul_readvariableop_resourceBsequential_gru_1_while_gru_cell_1_matmul_readvariableop_resource_0"x
9sequential_gru_1_while_gru_cell_1_readvariableop_resource;sequential_gru_1_while_gru_cell_1_readvariableop_resource_0"K
sequential_gru_1_while_identity(sequential/gru_1/while/Identity:output:0"O
!sequential_gru_1_while_identity_1*sequential/gru_1/while/Identity_1:output:0"O
!sequential_gru_1_while_identity_2*sequential/gru_1/while/Identity_2:output:0"O
!sequential_gru_1_while_identity_3*sequential/gru_1/while/Identity_3:output:0"O
!sequential_gru_1_while_identity_4*sequential/gru_1/while/Identity_4:output:0"t
7sequential_gru_1_while_sequential_gru_1_strided_slice_19sequential_gru_1_while_sequential_gru_1_strided_slice_1_0"м
ssequential_gru_1_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_1_tensorarrayunstack_tensorlistfromtensorusequential_gru_1_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_1_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2r
7sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOp7sequential/gru_1/while/gru_cell_1/MatMul/ReadVariableOp2v
9sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp9sequential/gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp2d
0sequential/gru_1/while/gru_cell_1/ReadVariableOp0sequential/gru_1/while/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
Я;
Е
C__inference_gru_1_layer_call_and_return_conditional_losses_25919322

inputs%
gru_cell_1_25919246:`&
gru_cell_1_25919248:	†`%
gru_cell_1_25919250: `
identityИҐ"gru_cell_1/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permД
	transpose	Transposeinputstranspose/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2т
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_1_25919246gru_cell_1_25919248gru_cell_1_25919250*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259192452$
"gru_cell_1/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЛ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_1_25919246gru_cell_1_25919248gru_cell_1_25919250*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25919258*
condR
while_cond_25919257*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity{
NoOpNoOp#^gru_cell_1/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall2
whilewhile:] Y
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
 
_user_specified_nameinputs
№
Ї
&__inference_gru_layer_call_fn_25921942
inputs_0
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identityИҐStatefulPartitionedCallО
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259187562
StatefulPartitionedCallЙ
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0
К;
ю
A__inference_gru_layer_call_and_return_conditional_losses_25918949

inputs$
gru_cell_25918873:	а$
gru_cell_25918875:	а%
gru_cell_25918877:
†а
identityИҐ gru_cell/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2и
 gru_cell/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_25918873gru_cell_25918875gru_cell_25918877*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259188222"
 gru_cell/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЗ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_25918873gru_cell_25918875gru_cell_25918877*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25918885*
condR
while_cond_25918884*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeт
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permѓ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimex
IdentityIdentitytranspose_1:y:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityy
NoOpNoOp!^gru_cell/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs
≤
Є
&__inference_gru_layer_call_fn_25921964

inputs
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identityИҐStatefulPartitionedCallГ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259199002
StatefulPartitionedCallА
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Д
і
while_cond_25922065
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25922065___redundant_placeholder06
2while_while_cond_25922065___redundant_placeholder16
2while_while_cond_25922065___redundant_placeholder26
2while_while_cond_25922065___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
а
d
E__inference_dropout_layer_call_and_return_conditional_losses_25921992

inputs
identityИc
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *д8О?2
dropout/Constx
dropout/MulMulinputsdropout/Const:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/MulT
dropout/ShapeShapeinputs*
T0*
_output_shapes
:2
dropout/Shape≈
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*,
_output_shapes
:€€€€€€€€€†*
dtype0*

seed02&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ=2
dropout/GreaterEqual/y√
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/GreaterEqualД
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:€€€€€€€€€†2
dropout/Cast
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/Mul_1j
IdentityIdentitydropout/Mul_1:z:0*
T0*,
_output_shapes
:€€€€€€€€€†2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
ВC
с
while_body_25919811
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
Ж
і
while_cond_25921841
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25921841___redundant_placeholder06
2while_while_cond_25921841___redundant_placeholder16
2while_while_cond_25921841___redundant_placeholder26
2while_while_cond_25921841___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Є"
∞
while_body_25918692
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_gru_cell_25918714_0:	а,
while_gru_cell_25918716_0:	а-
while_gru_cell_25918718_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_gru_cell_25918714:	а*
while_gru_cell_25918716:	а+
while_gru_cell_25918718:
†аИҐ&while/gru_cell/StatefulPartitionedCall√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem©
&while/gru_cell/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_gru_cell_25918714_0while_gru_cell_25918716_0while_gru_cell_25918718_0*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259186792(
&while/gru_cell/StatefulPartitionedCallу
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder/while/gru_cell/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3°
while/Identity_4Identity/while/gru_cell/StatefulPartitionedCall:output:1^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4Г

while/NoOpNoOp'^while/gru_cell/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"4
while_gru_cell_25918714while_gru_cell_25918714_0"4
while_gru_cell_25918716while_gru_cell_25918716_0"4
while_gru_cell_25918718while_gru_cell_25918718_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2P
&while/gru_cell/StatefulPartitionedCall&while/gru_cell/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
¬
є
(__inference_gru_1_layer_call_fn_25922625
inputs_0
unknown:`
	unknown_0:	†`
	unknown_1: `
identityИҐStatefulPartitionedCallВ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259193222
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:_ [
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
"
_user_specified_name
inputs/0
цW
Б
C__inference_gru_1_layer_call_and_return_conditional_losses_25922461

inputs4
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm{
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25922372*
condR
while_cond_25922371*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeи
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm•
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
у
ў
F__inference_gru_cell_layer_call_and_return_conditional_losses_25918679

inputs

states*
readvariableop_resource:	а1
matmul_readvariableop_resource:	а4
 matmul_1_readvariableop_resource:
†а
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpy
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes
:	а*
dtype02
ReadVariableOpl
unstackUnpackReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	а*
dtype02
MatMul/ReadVariableOpt
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
MatMult
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimѓ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
splitХ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02
MatMul_1/ReadVariableOpz
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2

MatMul_1z
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim‘
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2	
split_1h
addAddV2split:output:0split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
addY
SigmoidSigmoidadd:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2	
Sigmoidl
add_1AddV2split:output:1split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
add_1_
	Sigmoid_1Sigmoid	add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	Sigmoid_1e
mulMulSigmoid_1:y:0split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
mulc
add_2AddV2split:output:2mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_2R
ReluRelu	add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
Relu]
mul_1MulSigmoid:y:0states*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xa
subSubsub/x:output:0Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sube
mul_2Mulsub:z:0Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_2`
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_3e
IdentityIdentity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identityi

Identity_1Identity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:PL
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_namestates
ї
—
"sequential_gru_while_cond_25918363:
6sequential_gru_while_sequential_gru_while_loop_counter@
<sequential_gru_while_sequential_gru_while_maximum_iterations$
 sequential_gru_while_placeholder&
"sequential_gru_while_placeholder_1&
"sequential_gru_while_placeholder_2<
8sequential_gru_while_less_sequential_gru_strided_slice_1T
Psequential_gru_while_sequential_gru_while_cond_25918363___redundant_placeholder0T
Psequential_gru_while_sequential_gru_while_cond_25918363___redundant_placeholder1T
Psequential_gru_while_sequential_gru_while_cond_25918363___redundant_placeholder2T
Psequential_gru_while_sequential_gru_while_cond_25918363___redundant_placeholder3!
sequential_gru_while_identity
ї
sequential/gru/while/LessLess sequential_gru_while_placeholder8sequential_gru_while_less_sequential_gru_strided_slice_1*
T0*
_output_shapes
: 2
sequential/gru/while/LessК
sequential/gru/while/IdentityIdentitysequential/gru/while/Less:z:0*
T0
*
_output_shapes
: 2
sequential/gru/while/Identity"G
sequential_gru_while_identity&sequential/gru/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Ж
c
E__inference_dropout_layer_call_and_return_conditional_losses_25919913

inputs

identity_1_
IdentityIdentityinputs*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityn

Identity_1IdentityIdentity:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2

Identity_1"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
їD
э
while_body_25922372
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
Ж
і
while_cond_25920395
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25920395___redundant_placeholder06
2while_while_cond_25920395___redundant_placeholder16
2while_while_cond_25920395___redundant_placeholder26
2while_while_cond_25920395___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Д
і
while_cond_25919257
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25919257___redundant_placeholder06
2while_while_cond_25919257___redundant_placeholder16
2while_while_cond_25919257___redundant_placeholder26
2while_while_cond_25919257___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
ы
џ
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922755

inputs
states_0*
readvariableop_resource:	а1
matmul_readvariableop_resource:	а4
 matmul_1_readvariableop_resource:
†а
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpy
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes
:	а*
dtype02
ReadVariableOpl
unstackUnpackReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	а*
dtype02
MatMul/ReadVariableOpt
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
MatMult
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimѓ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
splitХ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02
MatMul_1/ReadVariableOp|
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2

MatMul_1z
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim‘
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2	
split_1h
addAddV2split:output:0split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
addY
SigmoidSigmoidadd:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2	
Sigmoidl
add_1AddV2split:output:1split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
add_1_
	Sigmoid_1Sigmoid	add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	Sigmoid_1e
mulMulSigmoid_1:y:0split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
mulc
add_2AddV2split:output:2mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_2R
ReluRelu	add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
Relu_
mul_1MulSigmoid:y:0states_0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xa
subSubsub/x:output:0Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sube
mul_2Mulsub:z:0Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_2`
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_3e
IdentityIdentity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identityi

Identity_1Identity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:RN
(
_output_shapes
:€€€€€€€€€†
"
_user_specified_name
states/0
цW
Б
C__inference_gru_1_layer_call_and_return_conditional_losses_25920067

inputs4
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm{
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25919978*
condR
while_cond_25919977*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeи
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm•
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
™
Ј
(__inference_gru_1_layer_call_fn_25922658

inputs
unknown:`
	unknown_0:	†`
	unknown_1: `
identityИҐStatefulPartitionedCallА
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259202872
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
їD
э
while_body_25922066
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
≈V
ц
A__inference_gru_layer_call_and_return_conditional_losses_25921778

inputs3
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25921689*
condR
while_cond_25921688*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeй
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm¶
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeo
IdentityIdentitytranspose_1:y:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
•

ф
C__inference_dense_layer_call_and_return_conditional_losses_25920085

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identityИҐBiasAdd/ReadVariableOpҐMatMul/ReadVariableOpН
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
MatMulМ
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOpБ
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:€€€€€€€€€ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs
Ж
і
while_cond_25921535
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25921535___redundant_placeholder06
2while_while_cond_25921535___redundant_placeholder16
2while_while_cond_25921535___redundant_placeholder26
2while_while_cond_25921535___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
сќ
М
H__inference_sequential_layer_call_and_return_conditional_losses_25921277

inputs7
$gru_gru_cell_readvariableop_resource:	а>
+gru_gru_cell_matmul_readvariableop_resource:	аA
-gru_gru_cell_matmul_1_readvariableop_resource:
†а:
(gru_1_gru_cell_1_readvariableop_resource:`B
/gru_1_gru_cell_1_matmul_readvariableop_resource:	†`C
1gru_1_gru_cell_1_matmul_1_readvariableop_resource: `6
$dense_matmul_readvariableop_resource: 3
%dense_biasadd_readvariableop_resource:
identityИҐdense/BiasAdd/ReadVariableOpҐdense/MatMul/ReadVariableOpҐ"gru/gru_cell/MatMul/ReadVariableOpҐ$gru/gru_cell/MatMul_1/ReadVariableOpҐgru/gru_cell/ReadVariableOpҐ	gru/whileҐ&gru_1/gru_cell_1/MatMul/ReadVariableOpҐ(gru_1/gru_cell_1/MatMul_1/ReadVariableOpҐgru_1/gru_cell_1/ReadVariableOpҐgru_1/whileL
	gru/ShapeShapeinputs*
T0*
_output_shapes
:2
	gru/Shape|
gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice/stackА
gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_1А
gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_2ъ
gru/strided_sliceStridedSlicegru/Shape:output:0 gru/strided_slice/stack:output:0"gru/strided_slice/stack_1:output:0"gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slicek
gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
gru/zeros/packed/1У
gru/zeros/packedPackgru/strided_slice:output:0gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru/zeros/packedg
gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/zeros/ConstЖ
	gru/zerosFillgru/zeros/packed:output:0gru/zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	gru/zeros}
gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose/permЖ
gru/transpose	Transposeinputsgru/transpose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
gru/transpose[
gru/Shape_1Shapegru/transpose:y:0*
T0*
_output_shapes
:2
gru/Shape_1А
gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_1/stackД
gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_1Д
gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_2Ж
gru/strided_slice_1StridedSlicegru/Shape_1:output:0"gru/strided_slice_1/stack:output:0$gru/strided_slice_1/stack_1:output:0$gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slice_1Н
gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2!
gru/TensorArrayV2/element_shape¬
gru/TensorArrayV2TensorListReserve(gru/TensorArrayV2/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2«
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2;
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeИ
+gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru/transpose:y:0Bgru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02-
+gru/TensorArrayUnstack/TensorListFromTensorА
gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_2/stackД
gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_1Д
gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_2Ф
gru/strided_slice_2StridedSlicegru/transpose:y:0"gru/strided_slice_2/stack:output:0$gru/strided_slice_2/stack_1:output:0$gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
gru/strided_slice_2†
gru/gru_cell/ReadVariableOpReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru/gru_cell/ReadVariableOpУ
gru/gru_cell/unstackUnpack#gru/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru/gru_cell/unstackµ
"gru/gru_cell/MatMul/ReadVariableOpReadVariableOp+gru_gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02$
"gru/gru_cell/MatMul/ReadVariableOp±
gru/gru_cell/MatMulMatMulgru/strided_slice_2:output:0*gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/MatMul®
gru/gru_cell/BiasAddBiasAddgru/gru_cell/MatMul:product:0gru/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/BiasAddЗ
gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru/gru_cell/split/split_dimг
gru/gru_cell/splitSplit%gru/gru_cell/split/split_dim:output:0gru/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/gru_cell/splitЉ
$gru/gru_cell/MatMul_1/ReadVariableOpReadVariableOp-gru_gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02&
$gru/gru_cell/MatMul_1/ReadVariableOp≠
gru/gru_cell/MatMul_1MatMulgru/zeros:output:0,gru/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/MatMul_1Ѓ
gru/gru_cell/BiasAdd_1BiasAddgru/gru_cell/MatMul_1:product:0gru/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/BiasAdd_1}
gru/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru/gru_cell/ConstЛ
gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru/gru_cell/split_1/split_dimХ
gru/gru_cell/split_1SplitVgru/gru_cell/BiasAdd_1:output:0gru/gru_cell/Const:output:0'gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/gru_cell/split_1Ь
gru/gru_cell/addAddV2gru/gru_cell/split:output:0gru/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/addА
gru/gru_cell/SigmoidSigmoidgru/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/Sigmoid†
gru/gru_cell/add_1AddV2gru/gru_cell/split:output:1gru/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_1Ж
gru/gru_cell/Sigmoid_1Sigmoidgru/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/Sigmoid_1Щ
gru/gru_cell/mulMulgru/gru_cell/Sigmoid_1:y:0gru/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mulЧ
gru/gru_cell/add_2AddV2gru/gru_cell/split:output:2gru/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_2y
gru/gru_cell/ReluRelugru/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/ReluР
gru/gru_cell/mul_1Mulgru/gru_cell/Sigmoid:y:0gru/zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mul_1m
gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru/gru_cell/sub/xХ
gru/gru_cell/subSubgru/gru_cell/sub/x:output:0gru/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/subЩ
gru/gru_cell/mul_2Mulgru/gru_cell/sub:z:0gru/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mul_2Ф
gru/gru_cell/add_3AddV2gru/gru_cell/mul_1:z:0gru/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_3Ч
!gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2#
!gru/TensorArrayV2_1/element_shape»
gru/TensorArrayV2_1TensorListReserve*gru/TensorArrayV2_1/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2_1V
gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru/timeЗ
gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru/while/maximum_iterationsr
gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/while/loop_counterь
	gru/whileWhilegru/while/loop_counter:output:0%gru/while/maximum_iterations:output:0gru/time:output:0gru/TensorArrayV2_1:handle:0gru/zeros:output:0gru/strided_slice_1:output:0;gru/TensorArrayUnstack/TensorListFromTensor:output_handle:0$gru_gru_cell_readvariableop_resource+gru_gru_cell_matmul_readvariableop_resource-gru_gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *#
bodyR
gru_while_body_25921025*#
condR
gru_while_cond_25921024*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
	gru/whileљ
4gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   26
4gru/TensorArrayV2Stack/TensorListStack/element_shapeщ
&gru/TensorArrayV2Stack/TensorListStackTensorListStackgru/while:output:3=gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02(
&gru/TensorArrayV2Stack/TensorListStackЙ
gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru/strided_slice_3/stackД
gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_3/stack_1Д
gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_3/stack_2≥
gru/strided_slice_3StridedSlice/gru/TensorArrayV2Stack/TensorListStack:tensor:0"gru/strided_slice_3/stack:output:0$gru/strided_slice_3/stack_1:output:0$gru/strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
gru/strided_slice_3Б
gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose_1/permґ
gru/transpose_1	Transpose/gru/TensorArrayV2Stack/TensorListStack:tensor:0gru/transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
gru/transpose_1n
gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/runtimes
dropout/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *д8О?2
dropout/dropout/ConstЭ
dropout/dropout/MulMulgru/transpose_1:y:0dropout/dropout/Const:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/dropout/Mulq
dropout/dropout/ShapeShapegru/transpose_1:y:0*
T0*
_output_shapes
:2
dropout/dropout/ShapeЁ
,dropout/dropout/random_uniform/RandomUniformRandomUniformdropout/dropout/Shape:output:0*
T0*,
_output_shapes
:€€€€€€€€€†*
dtype0*

seed02.
,dropout/dropout/random_uniform/RandomUniformЕ
dropout/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ=2 
dropout/dropout/GreaterEqual/yг
dropout/dropout/GreaterEqualGreaterEqual5dropout/dropout/random_uniform/RandomUniform:output:0'dropout/dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/dropout/GreaterEqualЬ
dropout/dropout/CastCast dropout/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:€€€€€€€€€†2
dropout/dropout/CastЯ
dropout/dropout/Mul_1Muldropout/dropout/Mul:z:0dropout/dropout/Cast:y:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/dropout/Mul_1c
gru_1/ShapeShapedropout/dropout/Mul_1:z:0*
T0*
_output_shapes
:2
gru_1/ShapeА
gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice/stackД
gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_1Д
gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_2Ж
gru_1/strided_sliceStridedSlicegru_1/Shape:output:0"gru_1/strided_slice/stack:output:0$gru_1/strided_slice/stack_1:output:0$gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slicen
gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/packed/1Ы
gru_1/zeros/packedPackgru_1/strided_slice:output:0gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru_1/zeros/packedk
gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/zeros/ConstН
gru_1/zerosFillgru_1/zeros/packed:output:0gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/zerosБ
gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose/perm†
gru_1/transpose	Transposedropout/dropout/Mul_1:z:0gru_1/transpose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
gru_1/transposea
gru_1/Shape_1Shapegru_1/transpose:y:0*
T0*
_output_shapes
:2
gru_1/Shape_1Д
gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_1/stackИ
gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_1И
gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_2Т
gru_1/strided_slice_1StridedSlicegru_1/Shape_1:output:0$gru_1/strided_slice_1/stack:output:0&gru_1/strided_slice_1/stack_1:output:0&gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slice_1С
!gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2#
!gru_1/TensorArrayV2/element_shape 
gru_1/TensorArrayV2TensorListReserve*gru_1/TensorArrayV2/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2Ћ
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2=
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeР
-gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru_1/transpose:y:0Dgru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02/
-gru_1/TensorArrayUnstack/TensorListFromTensorД
gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_2/stackИ
gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_1И
gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_2°
gru_1/strided_slice_2StridedSlicegru_1/transpose:y:0$gru_1/strided_slice_2/stack:output:0&gru_1/strided_slice_2/stack_1:output:0&gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
gru_1/strided_slice_2Ђ
gru_1/gru_cell_1/ReadVariableOpReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02!
gru_1/gru_cell_1/ReadVariableOpЭ
gru_1/gru_cell_1/unstackUnpack'gru_1/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_1/gru_cell_1/unstackЅ
&gru_1/gru_cell_1/MatMul/ReadVariableOpReadVariableOp/gru_1_gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02(
&gru_1/gru_cell_1/MatMul/ReadVariableOpЊ
gru_1/gru_cell_1/MatMulMatMulgru_1/strided_slice_2:output:0.gru_1/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/MatMulЈ
gru_1/gru_cell_1/BiasAddBiasAdd!gru_1/gru_cell_1/MatMul:product:0!gru_1/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/BiasAddП
 gru_1/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 gru_1/gru_cell_1/split/split_dimр
gru_1/gru_cell_1/splitSplit)gru_1/gru_cell_1/split/split_dim:output:0!gru_1/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/gru_cell_1/split∆
(gru_1/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp1gru_1_gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02*
(gru_1/gru_cell_1/MatMul_1/ReadVariableOpЇ
gru_1/gru_cell_1/MatMul_1MatMulgru_1/zeros:output:00gru_1/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/MatMul_1љ
gru_1/gru_cell_1/BiasAdd_1BiasAdd#gru_1/gru_cell_1/MatMul_1:product:0!gru_1/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/BiasAdd_1Е
gru_1/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_1/gru_cell_1/ConstУ
"gru_1/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"gru_1/gru_cell_1/split_1/split_dim¶
gru_1/gru_cell_1/split_1SplitV#gru_1/gru_cell_1/BiasAdd_1:output:0gru_1/gru_cell_1/Const:output:0+gru_1/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/gru_cell_1/split_1Ђ
gru_1/gru_cell_1/addAddV2gru_1/gru_cell_1/split:output:0!gru_1/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/addЛ
gru_1/gru_cell_1/SigmoidSigmoidgru_1/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Sigmoidѓ
gru_1/gru_cell_1/add_1AddV2gru_1/gru_cell_1/split:output:1!gru_1/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_1С
gru_1/gru_cell_1/Sigmoid_1Sigmoidgru_1/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Sigmoid_1®
gru_1/gru_cell_1/mulMulgru_1/gru_cell_1/Sigmoid_1:y:0!gru_1/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul¶
gru_1/gru_cell_1/add_2AddV2gru_1/gru_cell_1/split:output:2gru_1/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_2Д
gru_1/gru_cell_1/ReluRelugru_1/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/ReluЭ
gru_1/gru_cell_1/mul_1Mulgru_1/gru_cell_1/Sigmoid:y:0gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_1u
gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/gru_cell_1/sub/x§
gru_1/gru_cell_1/subSubgru_1/gru_cell_1/sub/x:output:0gru_1/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/sub®
gru_1/gru_cell_1/mul_2Mulgru_1/gru_cell_1/sub:z:0#gru_1/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_2£
gru_1/gru_cell_1/add_3AddV2gru_1/gru_cell_1/mul_1:z:0gru_1/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_3Ы
#gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2%
#gru_1/TensorArrayV2_1/element_shape–
gru_1/TensorArrayV2_1TensorListReserve,gru_1/TensorArrayV2_1/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2_1Z

gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru_1/timeЛ
gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru_1/while/maximum_iterationsv
gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/while/loop_counterЬ
gru_1/whileWhile!gru_1/while/loop_counter:output:0'gru_1/while/maximum_iterations:output:0gru_1/time:output:0gru_1/TensorArrayV2_1:handle:0gru_1/zeros:output:0gru_1/strided_slice_1:output:0=gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:0(gru_1_gru_cell_1_readvariableop_resource/gru_1_gru_cell_1_matmul_readvariableop_resource1gru_1_gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *%
bodyR
gru_1_while_body_25921182*%
condR
gru_1_while_cond_25921181*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
gru_1/whileЅ
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    28
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeА
(gru_1/TensorArrayV2Stack/TensorListStackTensorListStackgru_1/while:output:3?gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02*
(gru_1/TensorArrayV2Stack/TensorListStackН
gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru_1/strided_slice_3/stackИ
gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_3/stack_1И
gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_3/stack_2Њ
gru_1/strided_slice_3StridedSlice1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0$gru_1/strided_slice_3/stack:output:0&gru_1/strided_slice_3/stack_1:output:0&gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
gru_1/strided_slice_3Е
gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose_1/permљ
gru_1/transpose_1	Transpose1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0gru_1/transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
gru_1/transpose_1r
gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/runtimeЯ
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense/MatMul/ReadVariableOpЭ
dense/MatMulMatMulgru_1/strided_slice_3:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense/MatMulЮ
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02
dense/BiasAdd/ReadVariableOpЩ
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense/BiasAddq
IdentityIdentitydense/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

IdentityЕ
NoOpNoOp^dense/BiasAdd/ReadVariableOp^dense/MatMul/ReadVariableOp#^gru/gru_cell/MatMul/ReadVariableOp%^gru/gru_cell/MatMul_1/ReadVariableOp^gru/gru_cell/ReadVariableOp
^gru/while'^gru_1/gru_cell_1/MatMul/ReadVariableOp)^gru_1/gru_cell_1/MatMul_1/ReadVariableOp ^gru_1/gru_cell_1/ReadVariableOp^gru_1/while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2<
dense/BiasAdd/ReadVariableOpdense/BiasAdd/ReadVariableOp2:
dense/MatMul/ReadVariableOpdense/MatMul/ReadVariableOp2H
"gru/gru_cell/MatMul/ReadVariableOp"gru/gru_cell/MatMul/ReadVariableOp2L
$gru/gru_cell/MatMul_1/ReadVariableOp$gru/gru_cell/MatMul_1/ReadVariableOp2:
gru/gru_cell/ReadVariableOpgru/gru_cell/ReadVariableOp2
	gru/while	gru/while2P
&gru_1/gru_cell_1/MatMul/ReadVariableOp&gru_1/gru_cell_1/MatMul/ReadVariableOp2T
(gru_1/gru_cell_1/MatMul_1/ReadVariableOp(gru_1/gru_cell_1/MatMul_1/ReadVariableOp2B
gru_1/gru_cell_1/ReadVariableOpgru_1/gru_cell_1/ReadVariableOp2
gru_1/whilegru_1/while:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
ы
А
gru_while_cond_25921024$
 gru_while_gru_while_loop_counter*
&gru_while_gru_while_maximum_iterations
gru_while_placeholder
gru_while_placeholder_1
gru_while_placeholder_2&
"gru_while_less_gru_strided_slice_1>
:gru_while_gru_while_cond_25921024___redundant_placeholder0>
:gru_while_gru_while_cond_25921024___redundant_placeholder1>
:gru_while_gru_while_cond_25921024___redundant_placeholder2>
:gru_while_gru_while_cond_25921024___redundant_placeholder3
gru_while_identity
Д
gru/while/LessLessgru_while_placeholder"gru_while_less_gru_strided_slice_1*
T0*
_output_shapes
: 2
gru/while/Lessi
gru/while/IdentityIdentitygru/while/Less:z:0*
T0
*
_output_shapes
: 2
gru/while/Identity"1
gru_while_identitygru/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Я
”
H__inference_sequential_layer_call_and_return_conditional_losses_25920605
	gru_input
gru_25920584:	а
gru_25920586:	а 
gru_25920588:
†а 
gru_1_25920592:`!
gru_1_25920594:	†` 
gru_1_25920596: ` 
dense_25920599: 
dense_25920601:
identityИҐdense/StatefulPartitionedCallҐgru/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallЩ
gru/StatefulPartitionedCallStatefulPartitionedCall	gru_inputgru_25920584gru_25920586gru_25920588*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259199002
gru/StatefulPartitionedCallц
dropout/PartitionedCallPartitionedCall$gru/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259199132
dropout/PartitionedCallЈ
gru_1/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0gru_1_25920592gru_1_25920594gru_1_25920596*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259200672
gru_1/StatefulPartitionedCallЂ
dense/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_25920599dense_25920601*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_dense_layer_call_and_return_conditional_losses_259200852
dense/StatefulPartitionedCallБ
IdentityIdentity&dense/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityђ
NoOpNoOp^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
ДW
ш
A__inference_gru_layer_call_and_return_conditional_losses_25921472
inputs_03
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЕ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25921383*
condR
while_cond_25921382*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeт
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permѓ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimex
IdentityIdentitytranspose_1:y:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0
Д
і
while_cond_25922371
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25922371___redundant_placeholder06
2while_while_cond_25922371___redundant_placeholder16
2while_while_cond_25922371___redundant_placeholder26
2while_while_cond_25922371___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
Д
і
while_cond_25919450
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25919450___redundant_placeholder06
2while_while_cond_25919450___redundant_placeholder16
2while_while_cond_25919450___redundant_placeholder26
2while_while_cond_25919450___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
цW
Б
C__inference_gru_1_layer_call_and_return_conditional_losses_25920287

inputs4
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm{
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25920198*
condR
while_cond_25920197*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeи
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm•
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
Д
і
while_cond_25922524
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25922524___redundant_placeholder06
2while_while_cond_25922524___redundant_placeholder16
2while_while_cond_25922524___redundant_placeholder26
2while_while_cond_25922524___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
Њ
т
H__inference_sequential_layer_call_and_return_conditional_losses_25920541

inputs
gru_25920520:	а
gru_25920522:	а 
gru_25920524:
†а 
gru_1_25920528:`!
gru_1_25920530:	†` 
gru_1_25920532: ` 
dense_25920535: 
dense_25920537:
identityИҐdense/StatefulPartitionedCallҐdropout/StatefulPartitionedCallҐgru/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallЦ
gru/StatefulPartitionedCallStatefulPartitionedCallinputsgru_25920520gru_25920522gru_25920524*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259204852
gru/StatefulPartitionedCallО
dropout/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259203162!
dropout/StatefulPartitionedCallњ
gru_1/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0gru_1_25920528gru_1_25920530gru_1_25920532*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259202872
gru_1/StatefulPartitionedCallЂ
dense/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_25920535dense_25920537*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_dense_layer_call_and_return_conditional_losses_259200852
dense/StatefulPartitionedCallБ
IdentityIdentity&dense/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityќ
NoOpNoOp^dense/StatefulPartitionedCall ^dropout/StatefulPartitionedCall^gru/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dropout/StatefulPartitionedCalldropout/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Ж
і
while_cond_25919810
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25919810___redundant_placeholder06
2while_while_cond_25919810___redundant_placeholder16
2while_while_cond_25919810___redundant_placeholder26
2while_while_cond_25919810___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
їD
э
while_body_25922525
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
ы
џ
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922716

inputs
states_0*
readvariableop_resource:	а1
matmul_readvariableop_resource:	а4
 matmul_1_readvariableop_resource:
†а
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpy
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes
:	а*
dtype02
ReadVariableOpl
unstackUnpackReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	а*
dtype02
MatMul/ReadVariableOpt
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
MatMult
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimѓ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
splitХ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02
MatMul_1/ReadVariableOp|
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2

MatMul_1z
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim‘
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2	
split_1h
addAddV2split:output:0split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
addY
SigmoidSigmoidadd:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2	
Sigmoidl
add_1AddV2split:output:1split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
add_1_
	Sigmoid_1Sigmoid	add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	Sigmoid_1e
mulMulSigmoid_1:y:0split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
mulc
add_2AddV2split:output:2mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_2R
ReluRelu	add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
Relu_
mul_1MulSigmoid:y:0states_0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xa
subSubsub/x:output:0Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sube
mul_2Mulsub:z:0Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
mul_2`
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
add_3e
IdentityIdentity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identityi

Identity_1Identity	add_3:z:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:RN
(
_output_shapes
:€€€€€€€€€†
"
_user_specified_name
states/0
ВC
с
while_body_25921689
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
ВC
с
while_body_25920396
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
÷
Ў
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25919245

inputs

states)
readvariableop_resource:`1
matmul_readvariableop_resource:	†`2
 matmul_1_readvariableop_resource: `
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOpj
unstackUnpackReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
MatMuls
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimђ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
splitУ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2

MatMul_1y
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim—
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addX
SigmoidSigmoidadd:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
Sigmoidk
add_1AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_1^
	Sigmoid_1Sigmoid	add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	Sigmoid_1d
mulMulSigmoid_1:y:0split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
mulb
add_2AddV2split:output:2mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2Q
ReluRelu	add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Relu\
mul_1MulSigmoid:y:0states*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/x`
subSubsub/x:output:0Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
subd
mul_2Mulsub:z:0Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_2_
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_3d
IdentityIdentity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh

Identity_1Identity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:OK
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_namestates
ђX
Г
C__inference_gru_1_layer_call_and_return_conditional_losses_25922308
inputs_04
"gru_cell_1_readvariableop_resource:`<
)gru_cell_1_matmul_readvariableop_resource:	†`=
+gru_cell_1_matmul_1_readvariableop_resource: `
identityИҐ gru_cell_1/MatMul/ReadVariableOpҐ"gru_cell_1/MatMul_1/ReadVariableOpҐgru_cell_1/ReadVariableOpҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЖ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpЛ
gru_cell_1/unstackUnpack!gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_cell_1/unstackѓ
 gru_cell_1/MatMul/ReadVariableOpReadVariableOp)gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02"
 gru_cell_1/MatMul/ReadVariableOp¶
gru_cell_1/MatMulMatMulstrided_slice_2:output:0(gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMulЯ
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAddГ
gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split/split_dimЎ
gru_cell_1/splitSplit#gru_cell_1/split/split_dim:output:0gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/splitі
"gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp+gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02$
"gru_cell_1/MatMul_1/ReadVariableOpҐ
gru_cell_1/MatMul_1MatMulzeros:output:0*gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/MatMul_1•
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_cell_1/BiasAdd_1y
gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_cell_1/ConstЗ
gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell_1/split_1/split_dimИ
gru_cell_1/split_1SplitVgru_cell_1/BiasAdd_1:output:0gru_cell_1/Const:output:0%gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_cell_1/split_1У
gru_cell_1/addAddV2gru_cell_1/split:output:0gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addy
gru_cell_1/SigmoidSigmoidgru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/SigmoidЧ
gru_cell_1/add_1AddV2gru_cell_1/split:output:1gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_1
gru_cell_1/Sigmoid_1Sigmoidgru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Sigmoid_1Р
gru_cell_1/mulMulgru_cell_1/Sigmoid_1:y:0gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mulО
gru_cell_1/add_2AddV2gru_cell_1/split:output:2gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2r
gru_cell_1/ReluRelugru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ReluЕ
gru_cell_1/mul_1Mulgru_cell_1/Sigmoid:y:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_1i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xМ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subР
gru_cell_1/mul_2Mulgru_cell_1/sub:z:0gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_2Л
gru_cell_1/add_3AddV2gru_cell_1/mul_1:z:0gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter»
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource)gru_cell_1_matmul_readvariableop_resource+gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25922219*
condR
while_cond_25922218*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityЇ
NoOpNoOp!^gru_cell_1/MatMul/ReadVariableOp#^gru_cell_1/MatMul_1/ReadVariableOp^gru_cell_1/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 2D
 gru_cell_1/MatMul/ReadVariableOp gru_cell_1/MatMul/ReadVariableOp2H
"gru_cell_1/MatMul_1/ReadVariableOp"gru_cell_1/MatMul_1/ReadVariableOp26
gru_cell_1/ReadVariableOpgru_cell_1/ReadVariableOp2
whilewhile:_ [
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
"
_user_specified_name
inputs/0
їD
э
while_body_25919978
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
•

ф
C__inference_dense_layer_call_and_return_conditional_losses_25922668

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identityИҐBiasAdd/ReadVariableOpҐMatMul/ReadVariableOpН
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
MatMulМ
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOpБ
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:€€€€€€€€€ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs
ДW
ш
A__inference_gru_layer_call_and_return_conditional_losses_25921625
inputs_03
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЕ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25921536*
condR
while_cond_25921535*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeт
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permѓ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimex
IdentityIdentitytranspose_1:y:0^NoOp*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:€€€€€€€€€€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0
п
Х
(__inference_dense_layer_call_fn_25922677

inputs
unknown: 
	unknown_0:
identityИҐStatefulPartitionedCallу
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_dense_layer_call_and_return_conditional_losses_259200852
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:€€€€€€€€€ : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs
Ып
∞
#__inference__wrapped_model_25918609
	gru_inputB
/sequential_gru_gru_cell_readvariableop_resource:	аI
6sequential_gru_gru_cell_matmul_readvariableop_resource:	аL
8sequential_gru_gru_cell_matmul_1_readvariableop_resource:
†аE
3sequential_gru_1_gru_cell_1_readvariableop_resource:`M
:sequential_gru_1_gru_cell_1_matmul_readvariableop_resource:	†`N
<sequential_gru_1_gru_cell_1_matmul_1_readvariableop_resource: `A
/sequential_dense_matmul_readvariableop_resource: >
0sequential_dense_biasadd_readvariableop_resource:
identityИҐ'sequential/dense/BiasAdd/ReadVariableOpҐ&sequential/dense/MatMul/ReadVariableOpҐ-sequential/gru/gru_cell/MatMul/ReadVariableOpҐ/sequential/gru/gru_cell/MatMul_1/ReadVariableOpҐ&sequential/gru/gru_cell/ReadVariableOpҐsequential/gru/whileҐ1sequential/gru_1/gru_cell_1/MatMul/ReadVariableOpҐ3sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOpҐ*sequential/gru_1/gru_cell_1/ReadVariableOpҐsequential/gru_1/whilee
sequential/gru/ShapeShape	gru_input*
T0*
_output_shapes
:2
sequential/gru/ShapeТ
"sequential/gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2$
"sequential/gru/strided_slice/stackЦ
$sequential/gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2&
$sequential/gru/strided_slice/stack_1Ц
$sequential/gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2&
$sequential/gru/strided_slice/stack_2Љ
sequential/gru/strided_sliceStridedSlicesequential/gru/Shape:output:0+sequential/gru/strided_slice/stack:output:0-sequential/gru/strided_slice/stack_1:output:0-sequential/gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
sequential/gru/strided_sliceБ
sequential/gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
sequential/gru/zeros/packed/1њ
sequential/gru/zeros/packedPack%sequential/gru/strided_slice:output:0&sequential/gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
sequential/gru/zeros/packed}
sequential/gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru/zeros/Const≤
sequential/gru/zerosFill$sequential/gru/zeros/packed:output:0#sequential/gru/zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/zerosУ
sequential/gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
sequential/gru/transpose/perm™
sequential/gru/transpose	Transpose	gru_input&sequential/gru/transpose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
sequential/gru/transpose|
sequential/gru/Shape_1Shapesequential/gru/transpose:y:0*
T0*
_output_shapes
:2
sequential/gru/Shape_1Ц
$sequential/gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2&
$sequential/gru/strided_slice_1/stackЪ
&sequential/gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_1/stack_1Ъ
&sequential/gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_1/stack_2»
sequential/gru/strided_slice_1StridedSlicesequential/gru/Shape_1:output:0-sequential/gru/strided_slice_1/stack:output:0/sequential/gru/strided_slice_1/stack_1:output:0/sequential/gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2 
sequential/gru/strided_slice_1£
*sequential/gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2,
*sequential/gru/TensorArrayV2/element_shapeо
sequential/gru/TensorArrayV2TensorListReserve3sequential/gru/TensorArrayV2/element_shape:output:0'sequential/gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
sequential/gru/TensorArrayV2Ё
Dsequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2F
Dsequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shapeі
6sequential/gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorsequential/gru/transpose:y:0Msequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type028
6sequential/gru/TensorArrayUnstack/TensorListFromTensorЦ
$sequential/gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2&
$sequential/gru/strided_slice_2/stackЪ
&sequential/gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_2/stack_1Ъ
&sequential/gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_2/stack_2÷
sequential/gru/strided_slice_2StridedSlicesequential/gru/transpose:y:0-sequential/gru/strided_slice_2/stack:output:0/sequential/gru/strided_slice_2/stack_1:output:0/sequential/gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2 
sequential/gru/strided_slice_2Ѕ
&sequential/gru/gru_cell/ReadVariableOpReadVariableOp/sequential_gru_gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02(
&sequential/gru/gru_cell/ReadVariableOpі
sequential/gru/gru_cell/unstackUnpack.sequential/gru/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2!
sequential/gru/gru_cell/unstack÷
-sequential/gru/gru_cell/MatMul/ReadVariableOpReadVariableOp6sequential_gru_gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02/
-sequential/gru/gru_cell/MatMul/ReadVariableOpЁ
sequential/gru/gru_cell/MatMulMatMul'sequential/gru/strided_slice_2:output:05sequential/gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2 
sequential/gru/gru_cell/MatMul‘
sequential/gru/gru_cell/BiasAddBiasAdd(sequential/gru/gru_cell/MatMul:product:0(sequential/gru/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2!
sequential/gru/gru_cell/BiasAddЭ
'sequential/gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2)
'sequential/gru/gru_cell/split/split_dimП
sequential/gru/gru_cell/splitSplit0sequential/gru/gru_cell/split/split_dim:output:0(sequential/gru/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
sequential/gru/gru_cell/splitЁ
/sequential/gru/gru_cell/MatMul_1/ReadVariableOpReadVariableOp8sequential_gru_gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype021
/sequential/gru/gru_cell/MatMul_1/ReadVariableOpў
 sequential/gru/gru_cell/MatMul_1MatMulsequential/gru/zeros:output:07sequential/gru/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2"
 sequential/gru/gru_cell/MatMul_1Џ
!sequential/gru/gru_cell/BiasAdd_1BiasAdd*sequential/gru/gru_cell/MatMul_1:product:0(sequential/gru/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2#
!sequential/gru/gru_cell/BiasAdd_1У
sequential/gru/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
sequential/gru/gru_cell/Const°
)sequential/gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2+
)sequential/gru/gru_cell/split_1/split_dimћ
sequential/gru/gru_cell/split_1SplitV*sequential/gru/gru_cell/BiasAdd_1:output:0&sequential/gru/gru_cell/Const:output:02sequential/gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2!
sequential/gru/gru_cell/split_1»
sequential/gru/gru_cell/addAddV2&sequential/gru/gru_cell/split:output:0(sequential/gru/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/add°
sequential/gru/gru_cell/SigmoidSigmoidsequential/gru/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2!
sequential/gru/gru_cell/Sigmoidћ
sequential/gru/gru_cell/add_1AddV2&sequential/gru/gru_cell/split:output:1(sequential/gru/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/add_1І
!sequential/gru/gru_cell/Sigmoid_1Sigmoid!sequential/gru/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2#
!sequential/gru/gru_cell/Sigmoid_1≈
sequential/gru/gru_cell/mulMul%sequential/gru/gru_cell/Sigmoid_1:y:0(sequential/gru/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/mul√
sequential/gru/gru_cell/add_2AddV2&sequential/gru/gru_cell/split:output:2sequential/gru/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/add_2Ъ
sequential/gru/gru_cell/ReluRelu!sequential/gru/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/ReluЉ
sequential/gru/gru_cell/mul_1Mul#sequential/gru/gru_cell/Sigmoid:y:0sequential/gru/zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/mul_1Г
sequential/gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sequential/gru/gru_cell/sub/xЅ
sequential/gru/gru_cell/subSub&sequential/gru/gru_cell/sub/x:output:0#sequential/gru/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/sub≈
sequential/gru/gru_cell/mul_2Mulsequential/gru/gru_cell/sub:z:0*sequential/gru/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/mul_2ј
sequential/gru/gru_cell/add_3AddV2!sequential/gru/gru_cell/mul_1:z:0!sequential/gru/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
sequential/gru/gru_cell/add_3≠
,sequential/gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2.
,sequential/gru/TensorArrayV2_1/element_shapeф
sequential/gru/TensorArrayV2_1TensorListReserve5sequential/gru/TensorArrayV2_1/element_shape:output:0'sequential/gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02 
sequential/gru/TensorArrayV2_1l
sequential/gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential/gru/timeЭ
'sequential/gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2)
'sequential/gru/while/maximum_iterationsИ
!sequential/gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2#
!sequential/gru/while/loop_counterЦ
sequential/gru/whileWhile*sequential/gru/while/loop_counter:output:00sequential/gru/while/maximum_iterations:output:0sequential/gru/time:output:0'sequential/gru/TensorArrayV2_1:handle:0sequential/gru/zeros:output:0'sequential/gru/strided_slice_1:output:0Fsequential/gru/TensorArrayUnstack/TensorListFromTensor:output_handle:0/sequential_gru_gru_cell_readvariableop_resource6sequential_gru_gru_cell_matmul_readvariableop_resource8sequential_gru_gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *.
body&R$
"sequential_gru_while_body_25918364*.
cond&R$
"sequential_gru_while_cond_25918363*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
sequential/gru/while”
?sequential/gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2A
?sequential/gru/TensorArrayV2Stack/TensorListStack/element_shape•
1sequential/gru/TensorArrayV2Stack/TensorListStackTensorListStacksequential/gru/while:output:3Hsequential/gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype023
1sequential/gru/TensorArrayV2Stack/TensorListStackЯ
$sequential/gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2&
$sequential/gru/strided_slice_3/stackЪ
&sequential/gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2(
&sequential/gru/strided_slice_3/stack_1Ъ
&sequential/gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_3/stack_2х
sequential/gru/strided_slice_3StridedSlice:sequential/gru/TensorArrayV2Stack/TensorListStack:tensor:0-sequential/gru/strided_slice_3/stack:output:0/sequential/gru/strided_slice_3/stack_1:output:0/sequential/gru/strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2 
sequential/gru/strided_slice_3Ч
sequential/gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2!
sequential/gru/transpose_1/permв
sequential/gru/transpose_1	Transpose:sequential/gru/TensorArrayV2Stack/TensorListStack:tensor:0(sequential/gru/transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
sequential/gru/transpose_1Д
sequential/gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru/runtimeЭ
sequential/dropout/IdentityIdentitysequential/gru/transpose_1:y:0*
T0*,
_output_shapes
:€€€€€€€€€†2
sequential/dropout/IdentityД
sequential/gru_1/ShapeShape$sequential/dropout/Identity:output:0*
T0*
_output_shapes
:2
sequential/gru_1/ShapeЦ
$sequential/gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2&
$sequential/gru_1/strided_slice/stackЪ
&sequential/gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru_1/strided_slice/stack_1Ъ
&sequential/gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru_1/strided_slice/stack_2»
sequential/gru_1/strided_sliceStridedSlicesequential/gru_1/Shape:output:0-sequential/gru_1/strided_slice/stack:output:0/sequential/gru_1/strided_slice/stack_1:output:0/sequential/gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2 
sequential/gru_1/strided_sliceД
sequential/gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2!
sequential/gru_1/zeros/packed/1«
sequential/gru_1/zeros/packedPack'sequential/gru_1/strided_slice:output:0(sequential/gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
sequential/gru_1/zeros/packedБ
sequential/gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru_1/zeros/Constє
sequential/gru_1/zerosFill&sequential/gru_1/zeros/packed:output:0%sequential/gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sequential/gru_1/zerosЧ
sequential/gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2!
sequential/gru_1/transpose/permћ
sequential/gru_1/transpose	Transpose$sequential/dropout/Identity:output:0(sequential/gru_1/transpose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
sequential/gru_1/transposeВ
sequential/gru_1/Shape_1Shapesequential/gru_1/transpose:y:0*
T0*
_output_shapes
:2
sequential/gru_1/Shape_1Ъ
&sequential/gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&sequential/gru_1/strided_slice_1/stackЮ
(sequential/gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential/gru_1/strided_slice_1/stack_1Ю
(sequential/gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential/gru_1/strided_slice_1/stack_2‘
 sequential/gru_1/strided_slice_1StridedSlice!sequential/gru_1/Shape_1:output:0/sequential/gru_1/strided_slice_1/stack:output:01sequential/gru_1/strided_slice_1/stack_1:output:01sequential/gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 sequential/gru_1/strided_slice_1І
,sequential/gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2.
,sequential/gru_1/TensorArrayV2/element_shapeц
sequential/gru_1/TensorArrayV2TensorListReserve5sequential/gru_1/TensorArrayV2/element_shape:output:0)sequential/gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02 
sequential/gru_1/TensorArrayV2б
Fsequential/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2H
Fsequential/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeЉ
8sequential/gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorsequential/gru_1/transpose:y:0Osequential/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02:
8sequential/gru_1/TensorArrayUnstack/TensorListFromTensorЪ
&sequential/gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&sequential/gru_1/strided_slice_2/stackЮ
(sequential/gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential/gru_1/strided_slice_2/stack_1Ю
(sequential/gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential/gru_1/strided_slice_2/stack_2г
 sequential/gru_1/strided_slice_2StridedSlicesequential/gru_1/transpose:y:0/sequential/gru_1/strided_slice_2/stack:output:01sequential/gru_1/strided_slice_2/stack_1:output:01sequential/gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2"
 sequential/gru_1/strided_slice_2ћ
*sequential/gru_1/gru_cell_1/ReadVariableOpReadVariableOp3sequential_gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02,
*sequential/gru_1/gru_cell_1/ReadVariableOpЊ
#sequential/gru_1/gru_cell_1/unstackUnpack2sequential/gru_1/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2%
#sequential/gru_1/gru_cell_1/unstackв
1sequential/gru_1/gru_cell_1/MatMul/ReadVariableOpReadVariableOp:sequential_gru_1_gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype023
1sequential/gru_1/gru_cell_1/MatMul/ReadVariableOpк
"sequential/gru_1/gru_cell_1/MatMulMatMul)sequential/gru_1/strided_slice_2:output:09sequential/gru_1/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2$
"sequential/gru_1/gru_cell_1/MatMulг
#sequential/gru_1/gru_cell_1/BiasAddBiasAdd,sequential/gru_1/gru_cell_1/MatMul:product:0,sequential/gru_1/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2%
#sequential/gru_1/gru_cell_1/BiasAdd•
+sequential/gru_1/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2-
+sequential/gru_1/gru_cell_1/split/split_dimЬ
!sequential/gru_1/gru_cell_1/splitSplit4sequential/gru_1/gru_cell_1/split/split_dim:output:0,sequential/gru_1/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2#
!sequential/gru_1/gru_cell_1/splitз
3sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp<sequential_gru_1_gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype025
3sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOpж
$sequential/gru_1/gru_cell_1/MatMul_1MatMulsequential/gru_1/zeros:output:0;sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2&
$sequential/gru_1/gru_cell_1/MatMul_1й
%sequential/gru_1/gru_cell_1/BiasAdd_1BiasAdd.sequential/gru_1/gru_cell_1/MatMul_1:product:0,sequential/gru_1/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2'
%sequential/gru_1/gru_cell_1/BiasAdd_1Ы
!sequential/gru_1/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2#
!sequential/gru_1/gru_cell_1/Const©
-sequential/gru_1/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2/
-sequential/gru_1/gru_cell_1/split_1/split_dimЁ
#sequential/gru_1/gru_cell_1/split_1SplitV.sequential/gru_1/gru_cell_1/BiasAdd_1:output:0*sequential/gru_1/gru_cell_1/Const:output:06sequential/gru_1/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2%
#sequential/gru_1/gru_cell_1/split_1„
sequential/gru_1/gru_cell_1/addAddV2*sequential/gru_1/gru_cell_1/split:output:0,sequential/gru_1/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2!
sequential/gru_1/gru_cell_1/addђ
#sequential/gru_1/gru_cell_1/SigmoidSigmoid#sequential/gru_1/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential/gru_1/gru_cell_1/Sigmoidџ
!sequential/gru_1/gru_cell_1/add_1AddV2*sequential/gru_1/gru_cell_1/split:output:1,sequential/gru_1/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/gru_cell_1/add_1≤
%sequential/gru_1/gru_cell_1/Sigmoid_1Sigmoid%sequential/gru_1/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2'
%sequential/gru_1/gru_cell_1/Sigmoid_1‘
sequential/gru_1/gru_cell_1/mulMul)sequential/gru_1/gru_cell_1/Sigmoid_1:y:0,sequential/gru_1/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2!
sequential/gru_1/gru_cell_1/mul“
!sequential/gru_1/gru_cell_1/add_2AddV2*sequential/gru_1/gru_cell_1/split:output:2#sequential/gru_1/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/gru_cell_1/add_2•
 sequential/gru_1/gru_cell_1/ReluRelu%sequential/gru_1/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 sequential/gru_1/gru_cell_1/Relu…
!sequential/gru_1/gru_cell_1/mul_1Mul'sequential/gru_1/gru_cell_1/Sigmoid:y:0sequential/gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/gru_cell_1/mul_1Л
!sequential/gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2#
!sequential/gru_1/gru_cell_1/sub/x–
sequential/gru_1/gru_cell_1/subSub*sequential/gru_1/gru_cell_1/sub/x:output:0'sequential/gru_1/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2!
sequential/gru_1/gru_cell_1/sub‘
!sequential/gru_1/gru_cell_1/mul_2Mul#sequential/gru_1/gru_cell_1/sub:z:0.sequential/gru_1/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/gru_cell_1/mul_2ѕ
!sequential/gru_1/gru_cell_1/add_3AddV2%sequential/gru_1/gru_cell_1/mul_1:z:0%sequential/gru_1/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential/gru_1/gru_cell_1/add_3±
.sequential/gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    20
.sequential/gru_1/TensorArrayV2_1/element_shapeь
 sequential/gru_1/TensorArrayV2_1TensorListReserve7sequential/gru_1/TensorArrayV2_1/element_shape:output:0)sequential/gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02"
 sequential/gru_1/TensorArrayV2_1p
sequential/gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential/gru_1/time°
)sequential/gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2+
)sequential/gru_1/while/maximum_iterationsМ
#sequential/gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2%
#sequential/gru_1/while/loop_counterґ
sequential/gru_1/whileWhile,sequential/gru_1/while/loop_counter:output:02sequential/gru_1/while/maximum_iterations:output:0sequential/gru_1/time:output:0)sequential/gru_1/TensorArrayV2_1:handle:0sequential/gru_1/zeros:output:0)sequential/gru_1/strided_slice_1:output:0Hsequential/gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:03sequential_gru_1_gru_cell_1_readvariableop_resource:sequential_gru_1_gru_cell_1_matmul_readvariableop_resource<sequential_gru_1_gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *0
body(R&
$sequential_gru_1_while_body_25918514*0
cond(R&
$sequential_gru_1_while_cond_25918513*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
sequential/gru_1/while„
Asequential/gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2C
Asequential/gru_1/TensorArrayV2Stack/TensorListStack/element_shapeђ
3sequential/gru_1/TensorArrayV2Stack/TensorListStackTensorListStacksequential/gru_1/while:output:3Jsequential/gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype025
3sequential/gru_1/TensorArrayV2Stack/TensorListStack£
&sequential/gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2(
&sequential/gru_1/strided_slice_3/stackЮ
(sequential/gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential/gru_1/strided_slice_3/stack_1Ю
(sequential/gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential/gru_1/strided_slice_3/stack_2А
 sequential/gru_1/strided_slice_3StridedSlice<sequential/gru_1/TensorArrayV2Stack/TensorListStack:tensor:0/sequential/gru_1/strided_slice_3/stack:output:01sequential/gru_1/strided_slice_3/stack_1:output:01sequential/gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2"
 sequential/gru_1/strided_slice_3Ы
!sequential/gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2#
!sequential/gru_1/transpose_1/permй
sequential/gru_1/transpose_1	Transpose<sequential/gru_1/TensorArrayV2Stack/TensorListStack:tensor:0*sequential/gru_1/transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
sequential/gru_1/transpose_1И
sequential/gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru_1/runtimeј
&sequential/dense/MatMul/ReadVariableOpReadVariableOp/sequential_dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02(
&sequential/dense/MatMul/ReadVariableOp…
sequential/dense/MatMulMatMul)sequential/gru_1/strided_slice_3:output:0.sequential/dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
sequential/dense/MatMulњ
'sequential/dense/BiasAdd/ReadVariableOpReadVariableOp0sequential_dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02)
'sequential/dense/BiasAdd/ReadVariableOp≈
sequential/dense/BiasAddBiasAdd!sequential/dense/MatMul:product:0/sequential/dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
sequential/dense/BiasAdd|
IdentityIdentity!sequential/dense/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityу
NoOpNoOp(^sequential/dense/BiasAdd/ReadVariableOp'^sequential/dense/MatMul/ReadVariableOp.^sequential/gru/gru_cell/MatMul/ReadVariableOp0^sequential/gru/gru_cell/MatMul_1/ReadVariableOp'^sequential/gru/gru_cell/ReadVariableOp^sequential/gru/while2^sequential/gru_1/gru_cell_1/MatMul/ReadVariableOp4^sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOp+^sequential/gru_1/gru_cell_1/ReadVariableOp^sequential/gru_1/while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2R
'sequential/dense/BiasAdd/ReadVariableOp'sequential/dense/BiasAdd/ReadVariableOp2P
&sequential/dense/MatMul/ReadVariableOp&sequential/dense/MatMul/ReadVariableOp2^
-sequential/gru/gru_cell/MatMul/ReadVariableOp-sequential/gru/gru_cell/MatMul/ReadVariableOp2b
/sequential/gru/gru_cell/MatMul_1/ReadVariableOp/sequential/gru/gru_cell/MatMul_1/ReadVariableOp2P
&sequential/gru/gru_cell/ReadVariableOp&sequential/gru/gru_cell/ReadVariableOp2,
sequential/gru/whilesequential/gru/while2f
1sequential/gru_1/gru_cell_1/MatMul/ReadVariableOp1sequential/gru_1/gru_cell_1/MatMul/ReadVariableOp2j
3sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOp3sequential/gru_1/gru_cell_1/MatMul_1/ReadVariableOp2X
*sequential/gru_1/gru_cell_1/ReadVariableOp*sequential/gru_1/gru_cell_1/ReadVariableOp20
sequential/gru_1/whilesequential/gru_1/while:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
їD
э
while_body_25920198
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0<
*while_gru_cell_1_readvariableop_resource_0:`D
1while_gru_cell_1_matmul_readvariableop_resource_0:	†`E
3while_gru_cell_1_matmul_1_readvariableop_resource_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor:
(while_gru_cell_1_readvariableop_resource:`B
/while_gru_cell_1_matmul_readvariableop_resource:	†`C
1while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ&while/gru_cell_1/MatMul/ReadVariableOpҐ(while/gru_cell_1/MatMul_1/ReadVariableOpҐwhile/gru_cell_1/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≠
while/gru_cell_1/ReadVariableOpReadVariableOp*while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02!
while/gru_cell_1/ReadVariableOpЭ
while/gru_cell_1/unstackUnpack'while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
while/gru_cell_1/unstack√
&while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp1while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02(
&while/gru_cell_1/MatMul/ReadVariableOp–
while/gru_cell_1/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0.while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMulЈ
while/gru_cell_1/BiasAddBiasAdd!while/gru_cell_1/MatMul:product:0!while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAddП
 while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell_1/split/split_dimр
while/gru_cell_1/splitSplit)while/gru_cell_1/split/split_dim:output:0!while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split»
(while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp3while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype02*
(while/gru_cell_1/MatMul_1/ReadVariableOpє
while/gru_cell_1/MatMul_1MatMulwhile_placeholder_20while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/MatMul_1љ
while/gru_cell_1/BiasAdd_1BiasAdd#while/gru_cell_1/MatMul_1:product:0!while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
while/gru_cell_1/BiasAdd_1Е
while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
while/gru_cell_1/ConstУ
"while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"while/gru_cell_1/split_1/split_dim¶
while/gru_cell_1/split_1SplitV#while/gru_cell_1/BiasAdd_1:output:0while/gru_cell_1/Const:output:0+while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
while/gru_cell_1/split_1Ђ
while/gru_cell_1/addAddV2while/gru_cell_1/split:output:0!while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/addЛ
while/gru_cell_1/SigmoidSigmoidwhile/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoidѓ
while/gru_cell_1/add_1AddV2while/gru_cell_1/split:output:1!while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_1С
while/gru_cell_1/Sigmoid_1Sigmoidwhile/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/Sigmoid_1®
while/gru_cell_1/mulMulwhile/gru_cell_1/Sigmoid_1:y:0!while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul¶
while/gru_cell_1/add_2AddV2while/gru_cell_1/split:output:2while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_2Д
while/gru_cell_1/ReluReluwhile/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/ReluЬ
while/gru_cell_1/mul_1Mulwhile/gru_cell_1/Sigmoid:y:0while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_1u
while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell_1/sub/x§
while/gru_cell_1/subSubwhile/gru_cell_1/sub/x:output:0while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/sub®
while/gru_cell_1/mul_2Mulwhile/gru_cell_1/sub:z:0#while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/mul_2£
while/gru_cell_1/add_3AddV2while/gru_cell_1/mul_1:z:0while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/gru_cell_1/add_3ё
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Л
while/Identity_4Identitywhile/gru_cell_1/add_3:z:0^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4–

while/NoOpNoOp'^while/gru_cell_1/MatMul/ReadVariableOp)^while/gru_cell_1/MatMul_1/ReadVariableOp ^while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"h
1while_gru_cell_1_matmul_1_readvariableop_resource3while_gru_cell_1_matmul_1_readvariableop_resource_0"d
/while_gru_cell_1_matmul_readvariableop_resource1while_gru_cell_1_matmul_readvariableop_resource_0"V
(while_gru_cell_1_readvariableop_resource*while_gru_cell_1_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2P
&while/gru_cell_1/MatMul/ReadVariableOp&while/gru_cell_1/MatMul/ReadVariableOp2T
(while/gru_cell_1/MatMul_1/ReadVariableOp(while/gru_cell_1/MatMul_1/ReadVariableOp2B
while/gru_cell_1/ReadVariableOpwhile/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
у
ч
$sequential_gru_1_while_cond_25918513>
:sequential_gru_1_while_sequential_gru_1_while_loop_counterD
@sequential_gru_1_while_sequential_gru_1_while_maximum_iterations&
"sequential_gru_1_while_placeholder(
$sequential_gru_1_while_placeholder_1(
$sequential_gru_1_while_placeholder_2@
<sequential_gru_1_while_less_sequential_gru_1_strided_slice_1X
Tsequential_gru_1_while_sequential_gru_1_while_cond_25918513___redundant_placeholder0X
Tsequential_gru_1_while_sequential_gru_1_while_cond_25918513___redundant_placeholder1X
Tsequential_gru_1_while_sequential_gru_1_while_cond_25918513___redundant_placeholder2X
Tsequential_gru_1_while_sequential_gru_1_while_cond_25918513___redundant_placeholder3#
sequential_gru_1_while_identity
≈
sequential/gru_1/while/LessLess"sequential_gru_1_while_placeholder<sequential_gru_1_while_less_sequential_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
sequential/gru_1/while/LessР
sequential/gru_1/while/IdentityIdentitysequential/gru_1/while/Less:z:0*
T0
*
_output_shapes
: 2!
sequential/gru_1/while/Identity"K
sequential_gru_1_while_identity(sequential/gru_1/while/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
ВC
с
while_body_25921842
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
у

Џ
-__inference_gru_cell_1_layer_call_fn_25922889

inputs
states_0
unknown:`
	unknown_0:	†`
	unknown_1: `
identity

identity_1ИҐStatefulPartitionedCall§
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259193882
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0
≤
Є
&__inference_gru_layer_call_fn_25921975

inputs
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identityИҐStatefulPartitionedCallГ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259204852
StatefulPartitionedCallА
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
≥	
¶
gru_1_while_cond_25921181(
$gru_1_while_gru_1_while_loop_counter.
*gru_1_while_gru_1_while_maximum_iterations
gru_1_while_placeholder
gru_1_while_placeholder_1
gru_1_while_placeholder_2*
&gru_1_while_less_gru_1_strided_slice_1B
>gru_1_while_gru_1_while_cond_25921181___redundant_placeholder0B
>gru_1_while_gru_1_while_cond_25921181___redundant_placeholder1B
>gru_1_while_gru_1_while_cond_25921181___redundant_placeholder2B
>gru_1_while_gru_1_while_cond_25921181___redundant_placeholder3
gru_1_while_identity
О
gru_1/while/LessLessgru_1_while_placeholder&gru_1_while_less_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
gru_1/while/Lesso
gru_1/while/IdentityIdentitygru_1/while/Less:z:0*
T0
*
_output_shapes
: 2
gru_1/while/Identity"5
gru_1_while_identitygru_1/while/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
ЕC
Ѓ
!__inference__traced_save_25922996
file_prefix+
'savev2_dense_kernel_read_readvariableop)
%savev2_dense_bias_read_readvariableop2
.savev2_gru_gru_cell_kernel_read_readvariableop<
8savev2_gru_gru_cell_recurrent_kernel_read_readvariableop0
,savev2_gru_gru_cell_bias_read_readvariableop6
2savev2_gru_1_gru_cell_1_kernel_read_readvariableop@
<savev2_gru_1_gru_cell_1_recurrent_kernel_read_readvariableop4
0savev2_gru_1_gru_cell_1_bias_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop-
)savev2_dense_kernel_m_read_readvariableop+
'savev2_dense_bias_m_read_readvariableop4
0savev2_gru_gru_cell_kernel_m_read_readvariableop>
:savev2_gru_gru_cell_recurrent_kernel_m_read_readvariableop2
.savev2_gru_gru_cell_bias_m_read_readvariableop8
4savev2_gru_1_gru_cell_1_kernel_m_read_readvariableopB
>savev2_gru_1_gru_cell_1_recurrent_kernel_m_read_readvariableop6
2savev2_gru_1_gru_cell_1_bias_m_read_readvariableop-
)savev2_dense_kernel_v_read_readvariableop+
'savev2_dense_bias_v_read_readvariableop4
0savev2_gru_gru_cell_kernel_v_read_readvariableop>
:savev2_gru_gru_cell_recurrent_kernel_v_read_readvariableop2
.savev2_gru_gru_cell_bias_v_read_readvariableop8
4savev2_gru_1_gru_cell_1_kernel_v_read_readvariableopB
>savev2_gru_1_gru_cell_1_recurrent_kernel_v_read_readvariableop6
2savev2_gru_1_gru_cell_1_bias_v_read_readvariableop
savev2_const

identity_1ИҐMergeV2CheckpointsП
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Constl
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part2	
Const_1Л
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard¶
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilenameЗ
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*Щ
valueПBМB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names¬
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*M
valueDBBB B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices¶
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0'savev2_dense_kernel_read_readvariableop%savev2_dense_bias_read_readvariableop.savev2_gru_gru_cell_kernel_read_readvariableop8savev2_gru_gru_cell_recurrent_kernel_read_readvariableop,savev2_gru_gru_cell_bias_read_readvariableop2savev2_gru_1_gru_cell_1_kernel_read_readvariableop<savev2_gru_1_gru_cell_1_recurrent_kernel_read_readvariableop0savev2_gru_1_gru_cell_1_bias_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop)savev2_dense_kernel_m_read_readvariableop'savev2_dense_bias_m_read_readvariableop0savev2_gru_gru_cell_kernel_m_read_readvariableop:savev2_gru_gru_cell_recurrent_kernel_m_read_readvariableop.savev2_gru_gru_cell_bias_m_read_readvariableop4savev2_gru_1_gru_cell_1_kernel_m_read_readvariableop>savev2_gru_1_gru_cell_1_recurrent_kernel_m_read_readvariableop2savev2_gru_1_gru_cell_1_bias_m_read_readvariableop)savev2_dense_kernel_v_read_readvariableop'savev2_dense_bias_v_read_readvariableop0savev2_gru_gru_cell_kernel_v_read_readvariableop:savev2_gru_gru_cell_recurrent_kernel_v_read_readvariableop.savev2_gru_gru_cell_bias_v_read_readvariableop4savev2_gru_1_gru_cell_1_kernel_v_read_readvariableop>savev2_gru_1_gru_cell_1_recurrent_kernel_v_read_readvariableop2savev2_gru_1_gru_cell_1_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *+
dtypes!
22
SaveV2Ї
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes°
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identity_

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: 2

Identity_1c
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"!

identity_1Identity_1:output:0*Ф
_input_shapesВ
€: : ::	а:
†а:	а:	†`: `:`: : : : : ::	а:
†а:	а:	†`: `:`: ::	а:
†а:	а:	†`: `:`: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

: : 

_output_shapes
::%!

_output_shapes
:	а:&"
 
_output_shapes
:
†а:%!

_output_shapes
:	а:%!

_output_shapes
:	†`:$ 

_output_shapes

: `:$ 

_output_shapes

:`:	

_output_shapes
: :


_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

: : 

_output_shapes
::%!

_output_shapes
:	а:&"
 
_output_shapes
:
†а:%!

_output_shapes
:	а:%!

_output_shapes
:	†`:$ 

_output_shapes

: `:$ 

_output_shapes

:`:$ 

_output_shapes

: : 

_output_shapes
::%!

_output_shapes
:	а:&"
 
_output_shapes
:
†а:%!

_output_shapes
:	а:%!

_output_shapes
:	†`:$ 

_output_shapes

: `:$ 

_output_shapes

:`:

_output_shapes
: 
еM
±	
gru_1_while_body_25921182(
$gru_1_while_gru_1_while_loop_counter.
*gru_1_while_gru_1_while_maximum_iterations
gru_1_while_placeholder
gru_1_while_placeholder_1
gru_1_while_placeholder_2'
#gru_1_while_gru_1_strided_slice_1_0c
_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0B
0gru_1_while_gru_cell_1_readvariableop_resource_0:`J
7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0:	†`K
9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0: `
gru_1_while_identity
gru_1_while_identity_1
gru_1_while_identity_2
gru_1_while_identity_3
gru_1_while_identity_4%
!gru_1_while_gru_1_strided_slice_1a
]gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor@
.gru_1_while_gru_cell_1_readvariableop_resource:`H
5gru_1_while_gru_cell_1_matmul_readvariableop_resource:	†`I
7gru_1_while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ,gru_1/while/gru_cell_1/MatMul/ReadVariableOpҐ.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpҐ%gru_1/while/gru_cell_1/ReadVariableOpѕ
=gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2?
=gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeш
/gru_1/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0gru_1_while_placeholderFgru_1/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype021
/gru_1/while/TensorArrayV2Read/TensorListGetItemњ
%gru_1/while/gru_cell_1/ReadVariableOpReadVariableOp0gru_1_while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02'
%gru_1/while/gru_cell_1/ReadVariableOpѓ
gru_1/while/gru_cell_1/unstackUnpack-gru_1/while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2 
gru_1/while/gru_cell_1/unstack’
,gru_1/while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02.
,gru_1/while/gru_cell_1/MatMul/ReadVariableOpи
gru_1/while/gru_cell_1/MatMulMatMul6gru_1/while/TensorArrayV2Read/TensorListGetItem:item:04gru_1/while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/while/gru_cell_1/MatMulѕ
gru_1/while/gru_cell_1/BiasAddBiasAdd'gru_1/while/gru_cell_1/MatMul:product:0'gru_1/while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2 
gru_1/while/gru_cell_1/BiasAddЫ
&gru_1/while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2(
&gru_1/while/gru_cell_1/split/split_dimИ
gru_1/while/gru_cell_1/splitSplit/gru_1/while/gru_cell_1/split/split_dim:output:0'gru_1/while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/while/gru_cell_1/splitЏ
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype020
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp—
gru_1/while/gru_cell_1/MatMul_1MatMulgru_1_while_placeholder_26gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2!
gru_1/while/gru_cell_1/MatMul_1’
 gru_1/while/gru_cell_1/BiasAdd_1BiasAdd)gru_1/while/gru_cell_1/MatMul_1:product:0'gru_1/while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2"
 gru_1/while/gru_cell_1/BiasAdd_1С
gru_1/while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_1/while/gru_cell_1/ConstЯ
(gru_1/while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2*
(gru_1/while/gru_cell_1/split_1/split_dimƒ
gru_1/while/gru_cell_1/split_1SplitV)gru_1/while/gru_cell_1/BiasAdd_1:output:0%gru_1/while/gru_cell_1/Const:output:01gru_1/while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2 
gru_1/while/gru_cell_1/split_1√
gru_1/while/gru_cell_1/addAddV2%gru_1/while/gru_cell_1/split:output:0'gru_1/while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/addЭ
gru_1/while/gru_cell_1/SigmoidSigmoidgru_1/while/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/while/gru_cell_1/Sigmoid«
gru_1/while/gru_cell_1/add_1AddV2%gru_1/while/gru_cell_1/split:output:1'gru_1/while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_1£
 gru_1/while/gru_cell_1/Sigmoid_1Sigmoid gru_1/while/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/while/gru_cell_1/Sigmoid_1ј
gru_1/while/gru_cell_1/mulMul$gru_1/while/gru_cell_1/Sigmoid_1:y:0'gru_1/while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mulЊ
gru_1/while/gru_cell_1/add_2AddV2%gru_1/while/gru_cell_1/split:output:2gru_1/while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_2Ц
gru_1/while/gru_cell_1/ReluRelu gru_1/while/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/Reluі
gru_1/while/gru_cell_1/mul_1Mul"gru_1/while/gru_cell_1/Sigmoid:y:0gru_1_while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mul_1Б
gru_1/while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/while/gru_cell_1/sub/xЉ
gru_1/while/gru_cell_1/subSub%gru_1/while/gru_cell_1/sub/x:output:0"gru_1/while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/subј
gru_1/while/gru_cell_1/mul_2Mulgru_1/while/gru_cell_1/sub:z:0)gru_1/while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mul_2ї
gru_1/while/gru_cell_1/add_3AddV2 gru_1/while/gru_cell_1/mul_1:z:0 gru_1/while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_3ь
0gru_1/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemgru_1_while_placeholder_1gru_1_while_placeholder gru_1/while/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype022
0gru_1/while/TensorArrayV2Write/TensorListSetItemh
gru_1/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru_1/while/add/yБ
gru_1/while/addAddV2gru_1_while_placeholdergru_1/while/add/y:output:0*
T0*
_output_shapes
: 2
gru_1/while/addl
gru_1/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru_1/while/add_1/yФ
gru_1/while/add_1AddV2$gru_1_while_gru_1_while_loop_countergru_1/while/add_1/y:output:0*
T0*
_output_shapes
: 2
gru_1/while/add_1Г
gru_1/while/IdentityIdentitygru_1/while/add_1:z:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/IdentityЬ
gru_1/while/Identity_1Identity*gru_1_while_gru_1_while_maximum_iterations^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_1Е
gru_1/while/Identity_2Identitygru_1/while/add:z:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_2≤
gru_1/while/Identity_3Identity@gru_1/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_3£
gru_1/while/Identity_4Identity gru_1/while/gru_cell_1/add_3:z:0^gru_1/while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/Identity_4о
gru_1/while/NoOpNoOp-^gru_1/while/gru_cell_1/MatMul/ReadVariableOp/^gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp&^gru_1/while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
gru_1/while/NoOp"H
!gru_1_while_gru_1_strided_slice_1#gru_1_while_gru_1_strided_slice_1_0"t
7gru_1_while_gru_cell_1_matmul_1_readvariableop_resource9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0"p
5gru_1_while_gru_cell_1_matmul_readvariableop_resource7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0"b
.gru_1_while_gru_cell_1_readvariableop_resource0gru_1_while_gru_cell_1_readvariableop_resource_0"5
gru_1_while_identitygru_1/while/Identity:output:0"9
gru_1_while_identity_1gru_1/while/Identity_1:output:0"9
gru_1_while_identity_2gru_1/while/Identity_2:output:0"9
gru_1_while_identity_3gru_1/while/Identity_3:output:0"9
gru_1_while_identity_4gru_1/while/Identity_4:output:0"ј
]gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2\
,gru_1/while/gru_cell_1/MatMul/ReadVariableOp,gru_1/while/gru_cell_1/MatMul/ReadVariableOp2`
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp2N
%gru_1/while/gru_cell_1/ReadVariableOp%gru_1/while/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
«
х
H__inference_sequential_layer_call_and_return_conditional_losses_25920629
	gru_input
gru_25920608:	а
gru_25920610:	а 
gru_25920612:
†а 
gru_1_25920616:`!
gru_1_25920618:	†` 
gru_1_25920620: ` 
dense_25920623: 
dense_25920625:
identityИҐdense/StatefulPartitionedCallҐdropout/StatefulPartitionedCallҐgru/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallЩ
gru/StatefulPartitionedCallStatefulPartitionedCall	gru_inputgru_25920608gru_25920610gru_25920612*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *J
fERC
A__inference_gru_layer_call_and_return_conditional_losses_259204852
gru/StatefulPartitionedCallО
dropout/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259203162!
dropout/StatefulPartitionedCallњ
gru_1/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0gru_1_25920616gru_1_25920618gru_1_25920620*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259202872
gru_1/StatefulPartitionedCallЂ
dense/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_25920623dense_25920625*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_dense_layer_call_and_return_conditional_losses_259200852
dense/StatefulPartitionedCallБ
IdentityIdentity&dense/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityќ
NoOpNoOp^dense/StatefulPartitionedCall ^dropout/StatefulPartitionedCall^gru/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dropout/StatefulPartitionedCalldropout/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
≈V
ц
A__inference_gru_layer_call_and_return_conditional_losses_25921931

inputs3
 gru_cell_readvariableop_resource:	а:
'gru_cell_matmul_readvariableop_resource:	а=
)gru_cell_matmul_1_readvariableop_resource:
†а
identityИҐgru_cell/MatMul/ReadVariableOpҐ gru_cell/MatMul_1/ReadVariableOpҐgru_cell/ReadVariableOpҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slicec
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constv
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2Ф
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru_cell/ReadVariableOpЗ
gru_cell/unstackUnpackgru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru_cell/unstack©
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02 
gru_cell/MatMul/ReadVariableOp°
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMulШ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split/split_dim”
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split∞
 gru_cell/MatMul_1/ReadVariableOpReadVariableOp)gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02"
 gru_cell/MatMul_1/ReadVariableOpЭ
gru_cell/MatMul_1MatMulzeros:output:0(gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/MatMul_1Ю
gru_cell/BiasAdd_1BiasAddgru_cell/MatMul_1:product:0gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru_cell/BiasAdd_1u
gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru_cell/ConstГ
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru_cell/split_1/split_dimБ
gru_cell/split_1SplitVgru_cell/BiasAdd_1:output:0gru_cell/Const:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru_cell/split_1М
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/addt
gru_cell/SigmoidSigmoidgru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/SigmoidР
gru_cell/add_1AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_1z
gru_cell/Sigmoid_1Sigmoidgru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/Sigmoid_1Й
gru_cell/mulMulgru_cell/Sigmoid_1:y:0gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mulЗ
gru_cell/add_2AddV2gru_cell/split:output:2gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_2m
gru_cell/ReluRelugru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/ReluА
gru_cell/mul_1Mulgru_cell/Sigmoid:y:0zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_1e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell/sub/xЕ
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/subЙ
gru_cell/mul_2Mulgru_cell/sub:z:0gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/mul_2Д
gru_cell/add_3AddV2gru_cell/mul_1:z:0gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru_cell/add_3П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterƒ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0 gru_cell_readvariableop_resource'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25921842*
condR
while_cond_25921841*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   22
0TensorArrayV2Stack/TensorListStack/element_shapeй
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ы
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm¶
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeo
IdentityIdentitytranspose_1:y:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityі
NoOpNoOp^gru_cell/MatMul/ReadVariableOp!^gru_cell/MatMul_1/ReadVariableOp^gru_cell/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*0
_input_shapes
:€€€€€€€€€: : : 2@
gru_cell/MatMul/ReadVariableOpgru_cell/MatMul/ReadVariableOp2D
 gru_cell/MatMul_1/ReadVariableOp gru_cell/MatMul_1/ReadVariableOp22
gru_cell/ReadVariableOpgru_cell/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Ж
і
while_cond_25918691
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25918691___redundant_placeholder06
2while_while_cond_25918691___redundant_placeholder16
2while_while_cond_25918691___redundant_placeholder26
2while_while_cond_25918691___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
™
Ј
(__inference_gru_1_layer_call_fn_25922647

inputs
unknown:`
	unknown_0:	†`
	unknown_1: `
identityИҐStatefulPartitionedCallА
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259200672
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
ч

џ
+__inference_gru_cell_layer_call_fn_25922783

inputs
states_0
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identity

identity_1ИҐStatefulPartitionedCall§
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259188222
StatefulPartitionedCall|
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

IdentityА

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:RN
(
_output_shapes
:€€€€€€€€€†
"
_user_specified_name
states/0
ё"
Є
while_body_25919258
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_gru_cell_1_25919280_0:`.
while_gru_cell_1_25919282_0:	†`-
while_gru_cell_1_25919284_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_gru_cell_1_25919280:`,
while_gru_cell_1_25919282:	†`+
while_gru_cell_1_25919284: `ИҐ(while/gru_cell_1/StatefulPartitionedCall√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≥
(while/gru_cell_1/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_gru_cell_1_25919280_0while_gru_cell_1_25919282_0while_gru_cell_1_25919284_0*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259192452*
(while/gru_cell_1/StatefulPartitionedCallх
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder1while/gru_cell_1/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Ґ
while/Identity_4Identity1while/gru_cell_1/StatefulPartitionedCall:output:1^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4Е

while/NoOpNoOp)^while/gru_cell_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"8
while_gru_cell_1_25919280while_gru_cell_1_25919280_0"8
while_gru_cell_1_25919282while_gru_cell_1_25919282_0"8
while_gru_cell_1_25919284while_gru_cell_1_25919284_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2T
(while/gru_cell_1/StatefulPartitionedCall(while/gru_cell_1/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
÷
Ў
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25919388

inputs

states)
readvariableop_resource:`1
matmul_readvariableop_resource:	†`2
 matmul_1_readvariableop_resource: `
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOpj
unstackUnpackReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
MatMuls
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimђ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
splitУ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2

MatMul_1y
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim—
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addX
SigmoidSigmoidadd:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
Sigmoidk
add_1AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_1^
	Sigmoid_1Sigmoid	add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	Sigmoid_1d
mulMulSigmoid_1:y:0split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
mulb
add_2AddV2split:output:2mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2Q
ReluRelu	add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Relu\
mul_1MulSigmoid:y:0states*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/x`
subSubsub/x:output:0Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
subd
mul_2Mulsub:z:0Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_2_
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_3d
IdentityIdentity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh

Identity_1Identity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:OK
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_namestates
еM
±	
gru_1_while_body_25920866(
$gru_1_while_gru_1_while_loop_counter.
*gru_1_while_gru_1_while_maximum_iterations
gru_1_while_placeholder
gru_1_while_placeholder_1
gru_1_while_placeholder_2'
#gru_1_while_gru_1_strided_slice_1_0c
_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0B
0gru_1_while_gru_cell_1_readvariableop_resource_0:`J
7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0:	†`K
9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0: `
gru_1_while_identity
gru_1_while_identity_1
gru_1_while_identity_2
gru_1_while_identity_3
gru_1_while_identity_4%
!gru_1_while_gru_1_strided_slice_1a
]gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor@
.gru_1_while_gru_cell_1_readvariableop_resource:`H
5gru_1_while_gru_cell_1_matmul_readvariableop_resource:	†`I
7gru_1_while_gru_cell_1_matmul_1_readvariableop_resource: `ИҐ,gru_1/while/gru_cell_1/MatMul/ReadVariableOpҐ.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpҐ%gru_1/while/gru_cell_1/ReadVariableOpѕ
=gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2?
=gru_1/while/TensorArrayV2Read/TensorListGetItem/element_shapeш
/gru_1/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0gru_1_while_placeholderFgru_1/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype021
/gru_1/while/TensorArrayV2Read/TensorListGetItemњ
%gru_1/while/gru_cell_1/ReadVariableOpReadVariableOp0gru_1_while_gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02'
%gru_1/while/gru_cell_1/ReadVariableOpѓ
gru_1/while/gru_cell_1/unstackUnpack-gru_1/while/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2 
gru_1/while/gru_cell_1/unstack’
,gru_1/while/gru_cell_1/MatMul/ReadVariableOpReadVariableOp7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0*
_output_shapes
:	†`*
dtype02.
,gru_1/while/gru_cell_1/MatMul/ReadVariableOpи
gru_1/while/gru_cell_1/MatMulMatMul6gru_1/while/TensorArrayV2Read/TensorListGetItem:item:04gru_1/while/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/while/gru_cell_1/MatMulѕ
gru_1/while/gru_cell_1/BiasAddBiasAdd'gru_1/while/gru_cell_1/MatMul:product:0'gru_1/while/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2 
gru_1/while/gru_cell_1/BiasAddЫ
&gru_1/while/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2(
&gru_1/while/gru_cell_1/split/split_dimИ
gru_1/while/gru_cell_1/splitSplit/gru_1/while/gru_cell_1/split/split_dim:output:0'gru_1/while/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/while/gru_cell_1/splitЏ
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0*
_output_shapes

: `*
dtype020
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp—
gru_1/while/gru_cell_1/MatMul_1MatMulgru_1_while_placeholder_26gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2!
gru_1/while/gru_cell_1/MatMul_1’
 gru_1/while/gru_cell_1/BiasAdd_1BiasAdd)gru_1/while/gru_cell_1/MatMul_1:product:0'gru_1/while/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2"
 gru_1/while/gru_cell_1/BiasAdd_1С
gru_1/while/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_1/while/gru_cell_1/ConstЯ
(gru_1/while/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2*
(gru_1/while/gru_cell_1/split_1/split_dimƒ
gru_1/while/gru_cell_1/split_1SplitV)gru_1/while/gru_cell_1/BiasAdd_1:output:0%gru_1/while/gru_cell_1/Const:output:01gru_1/while/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2 
gru_1/while/gru_cell_1/split_1√
gru_1/while/gru_cell_1/addAddV2%gru_1/while/gru_cell_1/split:output:0'gru_1/while/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/addЭ
gru_1/while/gru_cell_1/SigmoidSigmoidgru_1/while/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/while/gru_cell_1/Sigmoid«
gru_1/while/gru_cell_1/add_1AddV2%gru_1/while/gru_cell_1/split:output:1'gru_1/while/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_1£
 gru_1/while/gru_cell_1/Sigmoid_1Sigmoid gru_1/while/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/while/gru_cell_1/Sigmoid_1ј
gru_1/while/gru_cell_1/mulMul$gru_1/while/gru_cell_1/Sigmoid_1:y:0'gru_1/while/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mulЊ
gru_1/while/gru_cell_1/add_2AddV2%gru_1/while/gru_cell_1/split:output:2gru_1/while/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_2Ц
gru_1/while/gru_cell_1/ReluRelu gru_1/while/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/Reluі
gru_1/while/gru_cell_1/mul_1Mul"gru_1/while/gru_cell_1/Sigmoid:y:0gru_1_while_placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mul_1Б
gru_1/while/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/while/gru_cell_1/sub/xЉ
gru_1/while/gru_cell_1/subSub%gru_1/while/gru_cell_1/sub/x:output:0"gru_1/while/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/subј
gru_1/while/gru_cell_1/mul_2Mulgru_1/while/gru_cell_1/sub:z:0)gru_1/while/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/mul_2ї
gru_1/while/gru_cell_1/add_3AddV2 gru_1/while/gru_cell_1/mul_1:z:0 gru_1/while/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/gru_cell_1/add_3ь
0gru_1/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemgru_1_while_placeholder_1gru_1_while_placeholder gru_1/while/gru_cell_1/add_3:z:0*
_output_shapes
: *
element_dtype022
0gru_1/while/TensorArrayV2Write/TensorListSetItemh
gru_1/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru_1/while/add/yБ
gru_1/while/addAddV2gru_1_while_placeholdergru_1/while/add/y:output:0*
T0*
_output_shapes
: 2
gru_1/while/addl
gru_1/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru_1/while/add_1/yФ
gru_1/while/add_1AddV2$gru_1_while_gru_1_while_loop_countergru_1/while/add_1/y:output:0*
T0*
_output_shapes
: 2
gru_1/while/add_1Г
gru_1/while/IdentityIdentitygru_1/while/add_1:z:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/IdentityЬ
gru_1/while/Identity_1Identity*gru_1_while_gru_1_while_maximum_iterations^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_1Е
gru_1/while/Identity_2Identitygru_1/while/add:z:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_2≤
gru_1/while/Identity_3Identity@gru_1/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^gru_1/while/NoOp*
T0*
_output_shapes
: 2
gru_1/while/Identity_3£
gru_1/while/Identity_4Identity gru_1/while/gru_cell_1/add_3:z:0^gru_1/while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/while/Identity_4о
gru_1/while/NoOpNoOp-^gru_1/while/gru_cell_1/MatMul/ReadVariableOp/^gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp&^gru_1/while/gru_cell_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
gru_1/while/NoOp"H
!gru_1_while_gru_1_strided_slice_1#gru_1_while_gru_1_strided_slice_1_0"t
7gru_1_while_gru_cell_1_matmul_1_readvariableop_resource9gru_1_while_gru_cell_1_matmul_1_readvariableop_resource_0"p
5gru_1_while_gru_cell_1_matmul_readvariableop_resource7gru_1_while_gru_cell_1_matmul_readvariableop_resource_0"b
.gru_1_while_gru_cell_1_readvariableop_resource0gru_1_while_gru_cell_1_readvariableop_resource_0"5
gru_1_while_identitygru_1/while/Identity:output:0"9
gru_1_while_identity_1gru_1/while/Identity_1:output:0"9
gru_1_while_identity_2gru_1/while/Identity_2:output:0"9
gru_1_while_identity_3gru_1/while/Identity_3:output:0"9
gru_1_while_identity_4gru_1/while/Identity_4:output:0"ј
]gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_gru_1_while_tensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2\
,gru_1/while/gru_cell_1/MatMul/ReadVariableOp,gru_1/while/gru_cell_1/MatMul/ReadVariableOp2`
.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp.gru_1/while/gru_cell_1/MatMul_1/ReadVariableOp2N
%gru_1/while/gru_cell_1/ReadVariableOp%gru_1/while/gru_cell_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
Ў≈
М
H__inference_sequential_layer_call_and_return_conditional_losses_25920961

inputs7
$gru_gru_cell_readvariableop_resource:	а>
+gru_gru_cell_matmul_readvariableop_resource:	аA
-gru_gru_cell_matmul_1_readvariableop_resource:
†а:
(gru_1_gru_cell_1_readvariableop_resource:`B
/gru_1_gru_cell_1_matmul_readvariableop_resource:	†`C
1gru_1_gru_cell_1_matmul_1_readvariableop_resource: `6
$dense_matmul_readvariableop_resource: 3
%dense_biasadd_readvariableop_resource:
identityИҐdense/BiasAdd/ReadVariableOpҐdense/MatMul/ReadVariableOpҐ"gru/gru_cell/MatMul/ReadVariableOpҐ$gru/gru_cell/MatMul_1/ReadVariableOpҐgru/gru_cell/ReadVariableOpҐ	gru/whileҐ&gru_1/gru_cell_1/MatMul/ReadVariableOpҐ(gru_1/gru_cell_1/MatMul_1/ReadVariableOpҐgru_1/gru_cell_1/ReadVariableOpҐgru_1/whileL
	gru/ShapeShapeinputs*
T0*
_output_shapes
:2
	gru/Shape|
gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice/stackА
gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_1А
gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_2ъ
gru/strided_sliceStridedSlicegru/Shape:output:0 gru/strided_slice/stack:output:0"gru/strided_slice/stack_1:output:0"gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slicek
gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value
B :†2
gru/zeros/packed/1У
gru/zeros/packedPackgru/strided_slice:output:0gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru/zeros/packedg
gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/zeros/ConstЖ
	gru/zerosFillgru/zeros/packed:output:0gru/zeros/Const:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
	gru/zeros}
gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose/permЖ
gru/transpose	Transposeinputsgru/transpose/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€2
gru/transpose[
gru/Shape_1Shapegru/transpose:y:0*
T0*
_output_shapes
:2
gru/Shape_1А
gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_1/stackД
gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_1Д
gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_2Ж
gru/strided_slice_1StridedSlicegru/Shape_1:output:0"gru/strided_slice_1/stack:output:0$gru/strided_slice_1/stack_1:output:0$gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slice_1Н
gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2!
gru/TensorArrayV2/element_shape¬
gru/TensorArrayV2TensorListReserve(gru/TensorArrayV2/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2«
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2;
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeИ
+gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru/transpose:y:0Bgru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02-
+gru/TensorArrayUnstack/TensorListFromTensorА
gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_2/stackД
gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_1Д
gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_2Ф
gru/strided_slice_2StridedSlicegru/transpose:y:0"gru/strided_slice_2/stack:output:0$gru/strided_slice_2/stack_1:output:0$gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
gru/strided_slice_2†
gru/gru_cell/ReadVariableOpReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes
:	а*
dtype02
gru/gru_cell/ReadVariableOpУ
gru/gru_cell/unstackUnpack#gru/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru/gru_cell/unstackµ
"gru/gru_cell/MatMul/ReadVariableOpReadVariableOp+gru_gru_cell_matmul_readvariableop_resource*
_output_shapes
:	а*
dtype02$
"gru/gru_cell/MatMul/ReadVariableOp±
gru/gru_cell/MatMulMatMulgru/strided_slice_2:output:0*gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/MatMul®
gru/gru_cell/BiasAddBiasAddgru/gru_cell/MatMul:product:0gru/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/BiasAddЗ
gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru/gru_cell/split/split_dimг
gru/gru_cell/splitSplit%gru/gru_cell/split/split_dim:output:0gru/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/gru_cell/splitЉ
$gru/gru_cell/MatMul_1/ReadVariableOpReadVariableOp-gru_gru_cell_matmul_1_readvariableop_resource* 
_output_shapes
:
†а*
dtype02&
$gru/gru_cell/MatMul_1/ReadVariableOp≠
gru/gru_cell/MatMul_1MatMulgru/zeros:output:0,gru/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/MatMul_1Ѓ
gru/gru_cell/BiasAdd_1BiasAddgru/gru_cell/MatMul_1:product:0gru/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/gru_cell/BiasAdd_1}
gru/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru/gru_cell/ConstЛ
gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru/gru_cell/split_1/split_dimХ
gru/gru_cell/split_1SplitVgru/gru_cell/BiasAdd_1:output:0gru/gru_cell/Const:output:0'gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/gru_cell/split_1Ь
gru/gru_cell/addAddV2gru/gru_cell/split:output:0gru/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/addА
gru/gru_cell/SigmoidSigmoidgru/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/Sigmoid†
gru/gru_cell/add_1AddV2gru/gru_cell/split:output:1gru/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_1Ж
gru/gru_cell/Sigmoid_1Sigmoidgru/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/Sigmoid_1Щ
gru/gru_cell/mulMulgru/gru_cell/Sigmoid_1:y:0gru/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mulЧ
gru/gru_cell/add_2AddV2gru/gru_cell/split:output:2gru/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_2y
gru/gru_cell/ReluRelugru/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/ReluР
gru/gru_cell/mul_1Mulgru/gru_cell/Sigmoid:y:0gru/zeros:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mul_1m
gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru/gru_cell/sub/xХ
gru/gru_cell/subSubgru/gru_cell/sub/x:output:0gru/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/subЩ
gru/gru_cell/mul_2Mulgru/gru_cell/sub:z:0gru/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/mul_2Ф
gru/gru_cell/add_3AddV2gru/gru_cell/mul_1:z:0gru/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/gru_cell/add_3Ч
!gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2#
!gru/TensorArrayV2_1/element_shape»
gru/TensorArrayV2_1TensorListReserve*gru/TensorArrayV2_1/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2_1V
gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru/timeЗ
gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
gru/while/maximum_iterationsr
gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/while/loop_counterь
	gru/whileWhilegru/while/loop_counter:output:0%gru/while/maximum_iterations:output:0gru/time:output:0gru/TensorArrayV2_1:handle:0gru/zeros:output:0gru/strided_slice_1:output:0;gru/TensorArrayUnstack/TensorListFromTensor:output_handle:0$gru_gru_cell_readvariableop_resource+gru_gru_cell_matmul_readvariableop_resource-gru_gru_cell_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*:
_output_shapes(
&: : : : :€€€€€€€€€†: : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *#
bodyR
gru_while_body_25920716*#
condR
gru_while_cond_25920715*9
output_shapes(
&: : : : :€€€€€€€€€†: : : : : *
parallel_iterations 2
	gru/whileљ
4gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   26
4gru/TensorArrayV2Stack/TensorListStack/element_shapeщ
&gru/TensorArrayV2Stack/TensorListStackTensorListStackgru/while:output:3=gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:€€€€€€€€€†*
element_dtype02(
&gru/TensorArrayV2Stack/TensorListStackЙ
gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru/strided_slice_3/stackД
gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_3/stack_1Д
gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_3/stack_2≥
gru/strided_slice_3StridedSlice/gru/TensorArrayV2Stack/TensorListStack:tensor:0"gru/strided_slice_3/stack:output:0$gru/strided_slice_3/stack_1:output:0$gru/strided_slice_3/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
gru/strided_slice_3Б
gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose_1/permґ
gru/transpose_1	Transpose/gru/TensorArrayV2Stack/TensorListStack:tensor:0gru/transpose_1/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
gru/transpose_1n
gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/runtime|
dropout/IdentityIdentitygru/transpose_1:y:0*
T0*,
_output_shapes
:€€€€€€€€€†2
dropout/Identityc
gru_1/ShapeShapedropout/Identity:output:0*
T0*
_output_shapes
:2
gru_1/ShapeА
gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice/stackД
gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_1Д
gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_2Ж
gru_1/strided_sliceStridedSlicegru_1/Shape:output:0"gru_1/strided_slice/stack:output:0$gru_1/strided_slice/stack_1:output:0$gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slicen
gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/packed/1Ы
gru_1/zeros/packedPackgru_1/strided_slice:output:0gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru_1/zeros/packedk
gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/zeros/ConstН
gru_1/zerosFillgru_1/zeros/packed:output:0gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/zerosБ
gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose/perm†
gru_1/transpose	Transposedropout/Identity:output:0gru_1/transpose/perm:output:0*
T0*,
_output_shapes
:€€€€€€€€€†2
gru_1/transposea
gru_1/Shape_1Shapegru_1/transpose:y:0*
T0*
_output_shapes
:2
gru_1/Shape_1Д
gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_1/stackИ
gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_1И
gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_2Т
gru_1/strided_slice_1StridedSlicegru_1/Shape_1:output:0$gru_1/strided_slice_1/stack:output:0&gru_1/strided_slice_1/stack_1:output:0&gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slice_1С
!gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2#
!gru_1/TensorArrayV2/element_shape 
gru_1/TensorArrayV2TensorListReserve*gru_1/TensorArrayV2/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2Ћ
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   2=
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeР
-gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru_1/transpose:y:0Dgru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02/
-gru_1/TensorArrayUnstack/TensorListFromTensorД
gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_2/stackИ
gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_1И
gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_2°
gru_1/strided_slice_2StridedSlicegru_1/transpose:y:0$gru_1/strided_slice_2/stack:output:0&gru_1/strided_slice_2/stack_1:output:0&gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
gru_1/strided_slice_2Ђ
gru_1/gru_cell_1/ReadVariableOpReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02!
gru_1/gru_cell_1/ReadVariableOpЭ
gru_1/gru_cell_1/unstackUnpack'gru_1/gru_cell_1/ReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2
gru_1/gru_cell_1/unstackЅ
&gru_1/gru_cell_1/MatMul/ReadVariableOpReadVariableOp/gru_1_gru_cell_1_matmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02(
&gru_1/gru_cell_1/MatMul/ReadVariableOpЊ
gru_1/gru_cell_1/MatMulMatMulgru_1/strided_slice_2:output:0.gru_1/gru_cell_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/MatMulЈ
gru_1/gru_cell_1/BiasAddBiasAdd!gru_1/gru_cell_1/MatMul:product:0!gru_1/gru_cell_1/unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/BiasAddП
 gru_1/gru_cell_1/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 gru_1/gru_cell_1/split/split_dimр
gru_1/gru_cell_1/splitSplit)gru_1/gru_cell_1/split/split_dim:output:0!gru_1/gru_cell_1/BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/gru_cell_1/split∆
(gru_1/gru_cell_1/MatMul_1/ReadVariableOpReadVariableOp1gru_1_gru_cell_1_matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02*
(gru_1/gru_cell_1/MatMul_1/ReadVariableOpЇ
gru_1/gru_cell_1/MatMul_1MatMulgru_1/zeros:output:00gru_1/gru_cell_1/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/MatMul_1љ
gru_1/gru_cell_1/BiasAdd_1BiasAdd#gru_1/gru_cell_1/MatMul_1:product:0!gru_1/gru_cell_1/unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
gru_1/gru_cell_1/BiasAdd_1Е
gru_1/gru_cell_1/ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
gru_1/gru_cell_1/ConstУ
"gru_1/gru_cell_1/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"gru_1/gru_cell_1/split_1/split_dim¶
gru_1/gru_cell_1/split_1SplitV#gru_1/gru_cell_1/BiasAdd_1:output:0gru_1/gru_cell_1/Const:output:0+gru_1/gru_cell_1/split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
gru_1/gru_cell_1/split_1Ђ
gru_1/gru_cell_1/addAddV2gru_1/gru_cell_1/split:output:0!gru_1/gru_cell_1/split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/addЛ
gru_1/gru_cell_1/SigmoidSigmoidgru_1/gru_cell_1/add:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Sigmoidѓ
gru_1/gru_cell_1/add_1AddV2gru_1/gru_cell_1/split:output:1!gru_1/gru_cell_1/split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_1С
gru_1/gru_cell_1/Sigmoid_1Sigmoidgru_1/gru_cell_1/add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Sigmoid_1®
gru_1/gru_cell_1/mulMulgru_1/gru_cell_1/Sigmoid_1:y:0!gru_1/gru_cell_1/split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul¶
gru_1/gru_cell_1/add_2AddV2gru_1/gru_cell_1/split:output:2gru_1/gru_cell_1/mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_2Д
gru_1/gru_cell_1/ReluRelugru_1/gru_cell_1/add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/ReluЭ
gru_1/gru_cell_1/mul_1Mulgru_1/gru_cell_1/Sigmoid:y:0gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_1u
gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/gru_cell_1/sub/x§
gru_1/gru_cell_1/subSubgru_1/gru_cell_1/sub/x:output:0gru_1/gru_cell_1/Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/sub®
gru_1/gru_cell_1/mul_2Mulgru_1/gru_cell_1/sub:z:0#gru_1/gru_cell_1/Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_2£
gru_1/gru_cell_1/add_3AddV2gru_1/gru_cell_1/mul_1:z:0gru_1/gru_cell_1/mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_3Ы
#gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2%
#gru_1/TensorArrayV2_1/element_shape–
gru_1/TensorArrayV2_1TensorListReserve,gru_1/TensorArrayV2_1/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2_1Z

gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru_1/timeЛ
gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru_1/while/maximum_iterationsv
gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/while/loop_counterЬ
gru_1/whileWhile!gru_1/while/loop_counter:output:0'gru_1/while/maximum_iterations:output:0gru_1/time:output:0gru_1/TensorArrayV2_1:handle:0gru_1/zeros:output:0gru_1/strided_slice_1:output:0=gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:0(gru_1_gru_cell_1_readvariableop_resource/gru_1_gru_cell_1_matmul_readvariableop_resource1gru_1_gru_cell_1_matmul_1_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *%
bodyR
gru_1_while_body_25920866*%
condR
gru_1_while_cond_25920865*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
gru_1/whileЅ
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    28
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeА
(gru_1/TensorArrayV2Stack/TensorListStackTensorListStackgru_1/while:output:3?gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:€€€€€€€€€ *
element_dtype02*
(gru_1/TensorArrayV2Stack/TensorListStackН
gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru_1/strided_slice_3/stackИ
gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_3/stack_1И
gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_3/stack_2Њ
gru_1/strided_slice_3StridedSlice1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0$gru_1/strided_slice_3/stack:output:0&gru_1/strided_slice_3/stack_1:output:0&gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
gru_1/strided_slice_3Е
gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose_1/permљ
gru_1/transpose_1	Transpose1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0gru_1/transpose_1/perm:output:0*
T0*+
_output_shapes
:€€€€€€€€€ 2
gru_1/transpose_1r
gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/runtimeЯ
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense/MatMul/ReadVariableOpЭ
dense/MatMulMatMulgru_1/strided_slice_3:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense/MatMulЮ
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02
dense/BiasAdd/ReadVariableOpЩ
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense/BiasAddq
IdentityIdentitydense/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

IdentityЕ
NoOpNoOp^dense/BiasAdd/ReadVariableOp^dense/MatMul/ReadVariableOp#^gru/gru_cell/MatMul/ReadVariableOp%^gru/gru_cell/MatMul_1/ReadVariableOp^gru/gru_cell/ReadVariableOp
^gru/while'^gru_1/gru_cell_1/MatMul/ReadVariableOp)^gru_1/gru_cell_1/MatMul_1/ReadVariableOp ^gru_1/gru_cell_1/ReadVariableOp^gru_1/while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 2<
dense/BiasAdd/ReadVariableOpdense/BiasAdd/ReadVariableOp2:
dense/MatMul/ReadVariableOpdense/MatMul/ReadVariableOp2H
"gru/gru_cell/MatMul/ReadVariableOp"gru/gru_cell/MatMul/ReadVariableOp2L
$gru/gru_cell/MatMul_1/ReadVariableOp$gru/gru_cell/MatMul_1/ReadVariableOp2:
gru/gru_cell/ReadVariableOpgru/gru_cell/ReadVariableOp2
	gru/while	gru/while2P
&gru_1/gru_cell_1/MatMul/ReadVariableOp&gru_1/gru_cell_1/MatMul/ReadVariableOp2T
(gru_1/gru_cell_1/MatMul_1/ReadVariableOp(gru_1/gru_cell_1/MatMul_1/ReadVariableOp2B
gru_1/gru_cell_1/ReadVariableOpgru_1/gru_cell_1/ReadVariableOp2
gru_1/whilegru_1/while:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Є"
∞
while_body_25918885
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_gru_cell_25918907_0:	а,
while_gru_cell_25918909_0:	а-
while_gru_cell_25918911_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_gru_cell_25918907:	а*
while_gru_cell_25918909:	а+
while_gru_cell_25918911:
†аИҐ&while/gru_cell/StatefulPartitionedCall√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem©
&while/gru_cell/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_gru_cell_25918907_0while_gru_cell_25918909_0while_gru_cell_25918911_0*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259188222(
&while/gru_cell/StatefulPartitionedCallу
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder/while/gru_cell/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3°
while/Identity_4Identity/while/gru_cell/StatefulPartitionedCall:output:1^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4Г

while/NoOpNoOp'^while/gru_cell/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"4
while_gru_cell_25918907while_gru_cell_25918907_0"4
while_gru_cell_25918909while_gru_cell_25918909_0"4
while_gru_cell_25918911while_gru_cell_25918911_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2P
&while/gru_cell/StatefulPartitionedCall&while/gru_cell/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
О

Ќ
-__inference_sequential_layer_call_fn_25921319

inputs
unknown:	а
	unknown_0:	а
	unknown_1:
†а
	unknown_2:`
	unknown_3:	†`
	unknown_4: `
	unknown_5: 
	unknown_6:
identityИҐStatefulPartitionedCall∆
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_sequential_layer_call_and_return_conditional_losses_259205412
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs
Ч

–
-__inference_sequential_layer_call_fn_25920581
	gru_input
unknown:	а
	unknown_0:	а
	unknown_1:
†а
	unknown_2:`
	unknown_3:	†`
	unknown_4: `
	unknown_5: 
	unknown_6:
identityИҐStatefulPartitionedCall…
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_sequential_layer_call_and_return_conditional_losses_259205412
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€: : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:V R
+
_output_shapes
:€€€€€€€€€
#
_user_specified_name	gru_input
§Z
≥
"sequential_gru_while_body_25918364:
6sequential_gru_while_sequential_gru_while_loop_counter@
<sequential_gru_while_sequential_gru_while_maximum_iterations$
 sequential_gru_while_placeholder&
"sequential_gru_while_placeholder_1&
"sequential_gru_while_placeholder_29
5sequential_gru_while_sequential_gru_strided_slice_1_0u
qsequential_gru_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0J
7sequential_gru_while_gru_cell_readvariableop_resource_0:	аQ
>sequential_gru_while_gru_cell_matmul_readvariableop_resource_0:	аT
@sequential_gru_while_gru_cell_matmul_1_readvariableop_resource_0:
†а!
sequential_gru_while_identity#
sequential_gru_while_identity_1#
sequential_gru_while_identity_2#
sequential_gru_while_identity_3#
sequential_gru_while_identity_47
3sequential_gru_while_sequential_gru_strided_slice_1s
osequential_gru_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensorH
5sequential_gru_while_gru_cell_readvariableop_resource:	аO
<sequential_gru_while_gru_cell_matmul_readvariableop_resource:	аR
>sequential_gru_while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ3sequential/gru/while/gru_cell/MatMul/ReadVariableOpҐ5sequential/gru/while/gru_cell/MatMul_1/ReadVariableOpҐ,sequential/gru/while/gru_cell/ReadVariableOpб
Fsequential/gru/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2H
Fsequential/gru/while/TensorArrayV2Read/TensorListGetItem/element_shape≠
8sequential/gru/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemqsequential_gru_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0 sequential_gru_while_placeholderOsequential/gru/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02:
8sequential/gru/while/TensorArrayV2Read/TensorListGetItem’
,sequential/gru/while/gru_cell/ReadVariableOpReadVariableOp7sequential_gru_while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02.
,sequential/gru/while/gru_cell/ReadVariableOp∆
%sequential/gru/while/gru_cell/unstackUnpack4sequential/gru/while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2'
%sequential/gru/while/gru_cell/unstackк
3sequential/gru/while/gru_cell/MatMul/ReadVariableOpReadVariableOp>sequential_gru_while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype025
3sequential/gru/while/gru_cell/MatMul/ReadVariableOpЗ
$sequential/gru/while/gru_cell/MatMulMatMul?sequential/gru/while/TensorArrayV2Read/TensorListGetItem:item:0;sequential/gru/while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2&
$sequential/gru/while/gru_cell/MatMulм
%sequential/gru/while/gru_cell/BiasAddBiasAdd.sequential/gru/while/gru_cell/MatMul:product:0.sequential/gru/while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2'
%sequential/gru/while/gru_cell/BiasAdd©
-sequential/gru/while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2/
-sequential/gru/while/gru_cell/split/split_dimІ
#sequential/gru/while/gru_cell/splitSplit6sequential/gru/while/gru_cell/split/split_dim:output:0.sequential/gru/while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2%
#sequential/gru/while/gru_cell/splitс
5sequential/gru/while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp@sequential_gru_while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype027
5sequential/gru/while/gru_cell/MatMul_1/ReadVariableOpр
&sequential/gru/while/gru_cell/MatMul_1MatMul"sequential_gru_while_placeholder_2=sequential/gru/while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2(
&sequential/gru/while/gru_cell/MatMul_1т
'sequential/gru/while/gru_cell/BiasAdd_1BiasAdd0sequential/gru/while/gru_cell/MatMul_1:product:0.sequential/gru/while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2)
'sequential/gru/while/gru_cell/BiasAdd_1Я
#sequential/gru/while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2%
#sequential/gru/while/gru_cell/Const≠
/sequential/gru/while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€21
/sequential/gru/while/gru_cell/split_1/split_dimк
%sequential/gru/while/gru_cell/split_1SplitV0sequential/gru/while/gru_cell/BiasAdd_1:output:0,sequential/gru/while/gru_cell/Const:output:08sequential/gru/while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2'
%sequential/gru/while/gru_cell/split_1а
!sequential/gru/while/gru_cell/addAddV2,sequential/gru/while/gru_cell/split:output:0.sequential/gru/while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2#
!sequential/gru/while/gru_cell/add≥
%sequential/gru/while/gru_cell/SigmoidSigmoid%sequential/gru/while/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2'
%sequential/gru/while/gru_cell/Sigmoidд
#sequential/gru/while/gru_cell/add_1AddV2,sequential/gru/while/gru_cell/split:output:1.sequential/gru/while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2%
#sequential/gru/while/gru_cell/add_1є
'sequential/gru/while/gru_cell/Sigmoid_1Sigmoid'sequential/gru/while/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2)
'sequential/gru/while/gru_cell/Sigmoid_1Ё
!sequential/gru/while/gru_cell/mulMul+sequential/gru/while/gru_cell/Sigmoid_1:y:0.sequential/gru/while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2#
!sequential/gru/while/gru_cell/mulџ
#sequential/gru/while/gru_cell/add_2AddV2,sequential/gru/while/gru_cell/split:output:2%sequential/gru/while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2%
#sequential/gru/while/gru_cell/add_2ђ
"sequential/gru/while/gru_cell/ReluRelu'sequential/gru/while/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2$
"sequential/gru/while/gru_cell/Relu”
#sequential/gru/while/gru_cell/mul_1Mul)sequential/gru/while/gru_cell/Sigmoid:y:0"sequential_gru_while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2%
#sequential/gru/while/gru_cell/mul_1П
#sequential/gru/while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2%
#sequential/gru/while/gru_cell/sub/xў
!sequential/gru/while/gru_cell/subSub,sequential/gru/while/gru_cell/sub/x:output:0)sequential/gru/while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2#
!sequential/gru/while/gru_cell/subЁ
#sequential/gru/while/gru_cell/mul_2Mul%sequential/gru/while/gru_cell/sub:z:00sequential/gru/while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2%
#sequential/gru/while/gru_cell/mul_2Ў
#sequential/gru/while/gru_cell/add_3AddV2'sequential/gru/while/gru_cell/mul_1:z:0'sequential/gru/while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2%
#sequential/gru/while/gru_cell/add_3І
9sequential/gru/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem"sequential_gru_while_placeholder_1 sequential_gru_while_placeholder'sequential/gru/while/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02;
9sequential/gru/while/TensorArrayV2Write/TensorListSetItemz
sequential/gru/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
sequential/gru/while/add/y•
sequential/gru/while/addAddV2 sequential_gru_while_placeholder#sequential/gru/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential/gru/while/add~
sequential/gru/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
sequential/gru/while/add_1/yЅ
sequential/gru/while/add_1AddV26sequential_gru_while_sequential_gru_while_loop_counter%sequential/gru/while/add_1/y:output:0*
T0*
_output_shapes
: 2
sequential/gru/while/add_1І
sequential/gru/while/IdentityIdentitysequential/gru/while/add_1:z:0^sequential/gru/while/NoOp*
T0*
_output_shapes
: 2
sequential/gru/while/Identity…
sequential/gru/while/Identity_1Identity<sequential_gru_while_sequential_gru_while_maximum_iterations^sequential/gru/while/NoOp*
T0*
_output_shapes
: 2!
sequential/gru/while/Identity_1©
sequential/gru/while/Identity_2Identitysequential/gru/while/add:z:0^sequential/gru/while/NoOp*
T0*
_output_shapes
: 2!
sequential/gru/while/Identity_2÷
sequential/gru/while/Identity_3IdentityIsequential/gru/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential/gru/while/NoOp*
T0*
_output_shapes
: 2!
sequential/gru/while/Identity_3∆
sequential/gru/while/Identity_4Identity'sequential/gru/while/gru_cell/add_3:z:0^sequential/gru/while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2!
sequential/gru/while/Identity_4Х
sequential/gru/while/NoOpNoOp4^sequential/gru/while/gru_cell/MatMul/ReadVariableOp6^sequential/gru/while/gru_cell/MatMul_1/ReadVariableOp-^sequential/gru/while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
sequential/gru/while/NoOp"В
>sequential_gru_while_gru_cell_matmul_1_readvariableop_resource@sequential_gru_while_gru_cell_matmul_1_readvariableop_resource_0"~
<sequential_gru_while_gru_cell_matmul_readvariableop_resource>sequential_gru_while_gru_cell_matmul_readvariableop_resource_0"p
5sequential_gru_while_gru_cell_readvariableop_resource7sequential_gru_while_gru_cell_readvariableop_resource_0"G
sequential_gru_while_identity&sequential/gru/while/Identity:output:0"K
sequential_gru_while_identity_1(sequential/gru/while/Identity_1:output:0"K
sequential_gru_while_identity_2(sequential/gru/while/Identity_2:output:0"K
sequential_gru_while_identity_3(sequential/gru/while/Identity_3:output:0"K
sequential_gru_while_identity_4(sequential/gru/while/Identity_4:output:0"l
3sequential_gru_while_sequential_gru_strided_slice_15sequential_gru_while_sequential_gru_strided_slice_1_0"д
osequential_gru_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensorqsequential_gru_while_tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2j
3sequential/gru/while/gru_cell/MatMul/ReadVariableOp3sequential/gru/while/gru_cell/MatMul/ReadVariableOp2n
5sequential/gru/while/gru_cell/MatMul_1/ReadVariableOp5sequential/gru/while/gru_cell/MatMul_1/ReadVariableOp2\
,sequential/gru/while/gru_cell/ReadVariableOp,sequential/gru/while/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
ы
А
gru_while_cond_25920715$
 gru_while_gru_while_loop_counter*
&gru_while_gru_while_maximum_iterations
gru_while_placeholder
gru_while_placeholder_1
gru_while_placeholder_2&
"gru_while_less_gru_strided_slice_1>
:gru_while_gru_while_cond_25920715___redundant_placeholder0>
:gru_while_gru_while_cond_25920715___redundant_placeholder1>
:gru_while_gru_while_cond_25920715___redundant_placeholder2>
:gru_while_gru_while_cond_25920715___redundant_placeholder3
gru_while_identity
Д
gru/while/LessLessgru_while_placeholder"gru_while_less_gru_strided_slice_1*
T0*
_output_shapes
: 2
gru/while/Lessi
gru/while/IdentityIdentitygru/while/Less:z:0*
T0
*
_output_shapes
: 2
gru/while/Identity"1
gru_while_identitygru/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Ж
і
while_cond_25918884
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25918884___redundant_placeholder06
2while_while_cond_25918884___redundant_placeholder16
2while_while_cond_25918884___redundant_placeholder26
2while_while_cond_25918884___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
ё
Џ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922861

inputs
states_0)
readvariableop_resource:`1
matmul_readvariableop_resource:	†`2
 matmul_1_readvariableop_resource: `
identity

identity_1ИҐMatMul/ReadVariableOpҐMatMul_1/ReadVariableOpҐReadVariableOpx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOpj
unstackUnpackReadVariableOp:value:0*
T0* 
_output_shapes
:`:`*	
num2	
unstackО
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	†`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2
MatMuls
BiasAddBiasAddMatMul:product:0unstack:output:0*
T0*'
_output_shapes
:€€€€€€€€€`2	
BiasAddm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split/split_dimђ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2
splitУ
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

: `*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€`2

MatMul_1y
	BiasAdd_1BiasAddMatMul_1:product:0unstack:output:1*
T0*'
_output_shapes
:€€€€€€€€€`2
	BiasAdd_1c
ConstConst*
_output_shapes
:*
dtype0*!
valueB"        €€€€2
Constq
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
split_1/split_dim—
split_1SplitVBiasAdd_1:output:0Const:output:0split_1/split_dim:output:0*
T0*

Tlen0*M
_output_shapes;
9:€€€€€€€€€ :€€€€€€€€€ :€€€€€€€€€ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addX
SigmoidSigmoidadd:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
Sigmoidk
add_1AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_1^
	Sigmoid_1Sigmoid	add_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	Sigmoid_1d
mulMulSigmoid_1:y:0split_1:output:2*
T0*'
_output_shapes
:€€€€€€€€€ 2
mulb
add_2AddV2split:output:2mul:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2Q
ReluRelu	add_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Relu^
mul_1MulSigmoid:y:0states_0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_1S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/x`
subSubsub/x:output:0Sigmoid:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
subd
mul_2Mulsub:z:0Relu:activations:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_2_
add_3AddV2	mul_1:z:0	mul_2:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_3d
IdentityIdentity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh

Identity_1Identity	add_3:z:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1С
NoOpNoOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€†:€€€€€€€€€ : : : 2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp2 
ReadVariableOpReadVariableOp:P L
(
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0
ВC
с
while_body_25921383
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0;
(while_gru_cell_readvariableop_resource_0:	аB
/while_gru_cell_matmul_readvariableop_resource_0:	аE
1while_gru_cell_matmul_1_readvariableop_resource_0:
†а
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor9
&while_gru_cell_readvariableop_resource:	а@
-while_gru_cell_matmul_readvariableop_resource:	аC
/while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ$while/gru_cell/MatMul/ReadVariableOpҐ&while/gru_cell/MatMul_1/ReadVariableOpҐwhile/gru_cell/ReadVariableOp√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape”
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem®
while/gru_cell/ReadVariableOpReadVariableOp(while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02
while/gru_cell/ReadVariableOpЩ
while/gru_cell/unstackUnpack%while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
while/gru_cell/unstackљ
$while/gru_cell/MatMul/ReadVariableOpReadVariableOp/while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02&
$while/gru_cell/MatMul/ReadVariableOpЋ
while/gru_cell/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0,while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul∞
while/gru_cell/BiasAddBiasAddwhile/gru_cell/MatMul:product:0while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAddЛ
while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
while/gru_cell/split/split_dimл
while/gru_cell/splitSplit'while/gru_cell/split/split_dim:output:0while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/splitƒ
&while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp1while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02(
&while/gru_cell/MatMul_1/ReadVariableOpі
while/gru_cell/MatMul_1MatMulwhile_placeholder_2.while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/MatMul_1ґ
while/gru_cell/BiasAdd_1BiasAdd!while/gru_cell/MatMul_1:product:0while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
while/gru_cell/BiasAdd_1Б
while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
while/gru_cell/ConstП
 while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2"
 while/gru_cell/split_1/split_dimЯ
while/gru_cell/split_1SplitV!while/gru_cell/BiasAdd_1:output:0while/gru_cell/Const:output:0)while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
while/gru_cell/split_1§
while/gru_cell/addAddV2while/gru_cell/split:output:0while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/addЖ
while/gru_cell/SigmoidSigmoidwhile/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid®
while/gru_cell/add_1AddV2while/gru_cell/split:output:1while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_1М
while/gru_cell/Sigmoid_1Sigmoidwhile/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/Sigmoid_1°
while/gru_cell/mulMulwhile/gru_cell/Sigmoid_1:y:0while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mulЯ
while/gru_cell/add_2AddV2while/gru_cell/split:output:2while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_2
while/gru_cell/ReluReluwhile/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/ReluЧ
while/gru_cell/mul_1Mulwhile/gru_cell/Sigmoid:y:0while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_1q
while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
while/gru_cell/sub/xЭ
while/gru_cell/subSubwhile/gru_cell/sub/x:output:0while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/sub°
while/gru_cell/mul_2Mulwhile/gru_cell/sub:z:0!while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/mul_2Ь
while/gru_cell/add_3AddV2while/gru_cell/mul_1:z:0while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
while/gru_cell/add_3№
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3К
while/Identity_4Identitywhile/gru_cell/add_3:z:0^while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
while/Identity_4 

while/NoOpNoOp%^while/gru_cell/MatMul/ReadVariableOp'^while/gru_cell/MatMul_1/ReadVariableOp^while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"d
/while_gru_cell_matmul_1_readvariableop_resource1while_gru_cell_matmul_1_readvariableop_resource_0"`
-while_gru_cell_matmul_readvariableop_resource/while_gru_cell_matmul_readvariableop_resource_0"R
&while_gru_cell_readvariableop_resource(while_gru_cell_readvariableop_resource_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2L
$while/gru_cell/MatMul/ReadVariableOp$while/gru_cell/MatMul/ReadVariableOp2P
&while/gru_cell/MatMul_1/ReadVariableOp&while/gru_cell/MatMul_1/ReadVariableOp2>
while/gru_cell/ReadVariableOpwhile/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
µ
c
*__inference_dropout_layer_call_fn_25922002

inputs
identityИҐStatefulPartitionedCallа
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:€€€€€€€€€†* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8В *N
fIRG
E__inference_dropout_layer_call_and_return_conditional_losses_259203162
StatefulPartitionedCallА
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*,
_output_shapes
:€€€€€€€€€†2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:€€€€€€€€€†22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:€€€€€€€€€†
 
_user_specified_nameinputs
ё"
Є
while_body_25919451
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_gru_cell_1_25919473_0:`.
while_gru_cell_1_25919475_0:	†`-
while_gru_cell_1_25919477_0: `
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_gru_cell_1_25919473:`,
while_gru_cell_1_25919475:	†`+
while_gru_cell_1_25919477: `ИҐ(while/gru_cell_1/StatefulPartitionedCall√
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape‘
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*(
_output_shapes
:€€€€€€€€€†*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem≥
(while/gru_cell_1/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_gru_cell_1_25919473_0while_gru_cell_1_25919475_0while_gru_cell_1_25919477_0*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259193882*
(while/gru_cell_1/StatefulPartitionedCallх
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder1while/gru_cell_1/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2Ъ
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3Ґ
while/Identity_4Identity1while/gru_cell_1/StatefulPartitionedCall:output:1^while/NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2
while/Identity_4Е

while/NoOpNoOp)^while/gru_cell_1/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp"8
while_gru_cell_1_25919473while_gru_cell_1_25919473_0"8
while_gru_cell_1_25919475while_gru_cell_1_25919475_0"8
while_gru_cell_1_25919477while_gru_cell_1_25919477_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"0
while_strided_slice_1while_strided_slice_1_0"®
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%: : : : :€€€€€€€€€ : : : : : 2T
(while/gru_cell_1/StatefulPartitionedCall(while/gru_cell_1/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: 
ЭI
й
gru_while_body_25921025$
 gru_while_gru_while_loop_counter*
&gru_while_gru_while_maximum_iterations
gru_while_placeholder
gru_while_placeholder_1
gru_while_placeholder_2#
gru_while_gru_strided_slice_1_0_
[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0?
,gru_while_gru_cell_readvariableop_resource_0:	аF
3gru_while_gru_cell_matmul_readvariableop_resource_0:	аI
5gru_while_gru_cell_matmul_1_readvariableop_resource_0:
†а
gru_while_identity
gru_while_identity_1
gru_while_identity_2
gru_while_identity_3
gru_while_identity_4!
gru_while_gru_strided_slice_1]
Ygru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor=
*gru_while_gru_cell_readvariableop_resource:	аD
1gru_while_gru_cell_matmul_readvariableop_resource:	аG
3gru_while_gru_cell_matmul_1_readvariableop_resource:
†аИҐ(gru/while/gru_cell/MatMul/ReadVariableOpҐ*gru/while/gru_cell/MatMul_1/ReadVariableOpҐ!gru/while/gru_cell/ReadVariableOpЋ
;gru/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2=
;gru/while/TensorArrayV2Read/TensorListGetItem/element_shapeл
-gru/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0gru_while_placeholderDgru/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02/
-gru/while/TensorArrayV2Read/TensorListGetItemі
!gru/while/gru_cell/ReadVariableOpReadVariableOp,gru_while_gru_cell_readvariableop_resource_0*
_output_shapes
:	а*
dtype02#
!gru/while/gru_cell/ReadVariableOp•
gru/while/gru_cell/unstackUnpack)gru/while/gru_cell/ReadVariableOp:value:0*
T0*"
_output_shapes
:а:а*	
num2
gru/while/gru_cell/unstack…
(gru/while/gru_cell/MatMul/ReadVariableOpReadVariableOp3gru_while_gru_cell_matmul_readvariableop_resource_0*
_output_shapes
:	а*
dtype02*
(gru/while/gru_cell/MatMul/ReadVariableOpџ
gru/while/gru_cell/MatMulMatMul4gru/while/TensorArrayV2Read/TensorListGetItem:item:00gru/while/gru_cell/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/MatMulј
gru/while/gru_cell/BiasAddBiasAdd#gru/while/gru_cell/MatMul:product:0#gru/while/gru_cell/unstack:output:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/BiasAddУ
"gru/while/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2$
"gru/while/gru_cell/split/split_dimы
gru/while/gru_cell/splitSplit+gru/while/gru_cell/split/split_dim:output:0#gru/while/gru_cell/BiasAdd:output:0*
T0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/while/gru_cell/split–
*gru/while/gru_cell/MatMul_1/ReadVariableOpReadVariableOp5gru_while_gru_cell_matmul_1_readvariableop_resource_0* 
_output_shapes
:
†а*
dtype02,
*gru/while/gru_cell/MatMul_1/ReadVariableOpƒ
gru/while/gru_cell/MatMul_1MatMulgru_while_placeholder_22gru/while/gru_cell/MatMul_1/ReadVariableOp:value:0*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/MatMul_1∆
gru/while/gru_cell/BiasAdd_1BiasAdd%gru/while/gru_cell/MatMul_1:product:0#gru/while/gru_cell/unstack:output:1*
T0*(
_output_shapes
:€€€€€€€€€а2
gru/while/gru_cell/BiasAdd_1Й
gru/while/gru_cell/ConstConst*
_output_shapes
:*
dtype0*!
valueB"†   †   €€€€2
gru/while/gru_cell/ConstЧ
$gru/while/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2&
$gru/while/gru_cell/split_1/split_dim≥
gru/while/gru_cell/split_1SplitV%gru/while/gru_cell/BiasAdd_1:output:0!gru/while/gru_cell/Const:output:0-gru/while/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*P
_output_shapes>
<:€€€€€€€€€†:€€€€€€€€€†:€€€€€€€€€†*
	num_split2
gru/while/gru_cell/split_1і
gru/while/gru_cell/addAddV2!gru/while/gru_cell/split:output:0#gru/while/gru_cell/split_1:output:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/addТ
gru/while/gru_cell/SigmoidSigmoidgru/while/gru_cell/add:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/SigmoidЄ
gru/while/gru_cell/add_1AddV2!gru/while/gru_cell/split:output:1#gru/while/gru_cell/split_1:output:1*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_1Ш
gru/while/gru_cell/Sigmoid_1Sigmoidgru/while/gru_cell/add_1:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/Sigmoid_1±
gru/while/gru_cell/mulMul gru/while/gru_cell/Sigmoid_1:y:0#gru/while/gru_cell/split_1:output:2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mulѓ
gru/while/gru_cell/add_2AddV2!gru/while/gru_cell/split:output:2gru/while/gru_cell/mul:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_2Л
gru/while/gru_cell/ReluRelugru/while/gru_cell/add_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/ReluІ
gru/while/gru_cell/mul_1Mulgru/while/gru_cell/Sigmoid:y:0gru_while_placeholder_2*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mul_1y
gru/while/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru/while/gru_cell/sub/x≠
gru/while/gru_cell/subSub!gru/while/gru_cell/sub/x:output:0gru/while/gru_cell/Sigmoid:y:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/sub±
gru/while/gru_cell/mul_2Mulgru/while/gru_cell/sub:z:0%gru/while/gru_cell/Relu:activations:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/mul_2ђ
gru/while/gru_cell/add_3AddV2gru/while/gru_cell/mul_1:z:0gru/while/gru_cell/mul_2:z:0*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/gru_cell/add_3р
.gru/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemgru_while_placeholder_1gru_while_placeholdergru/while/gru_cell/add_3:z:0*
_output_shapes
: *
element_dtype020
.gru/while/TensorArrayV2Write/TensorListSetItemd
gru/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru/while/add/yy
gru/while/addAddV2gru_while_placeholdergru/while/add/y:output:0*
T0*
_output_shapes
: 2
gru/while/addh
gru/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
gru/while/add_1/yК
gru/while/add_1AddV2 gru_while_gru_while_loop_countergru/while/add_1/y:output:0*
T0*
_output_shapes
: 2
gru/while/add_1{
gru/while/IdentityIdentitygru/while/add_1:z:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/IdentityТ
gru/while/Identity_1Identity&gru_while_gru_while_maximum_iterations^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_1}
gru/while/Identity_2Identitygru/while/add:z:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_2™
gru/while/Identity_3Identity>gru/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^gru/while/NoOp*
T0*
_output_shapes
: 2
gru/while/Identity_3Ъ
gru/while/Identity_4Identitygru/while/gru_cell/add_3:z:0^gru/while/NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2
gru/while/Identity_4ё
gru/while/NoOpNoOp)^gru/while/gru_cell/MatMul/ReadVariableOp+^gru/while/gru_cell/MatMul_1/ReadVariableOp"^gru/while/gru_cell/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
gru/while/NoOp"l
3gru_while_gru_cell_matmul_1_readvariableop_resource5gru_while_gru_cell_matmul_1_readvariableop_resource_0"h
1gru_while_gru_cell_matmul_readvariableop_resource3gru_while_gru_cell_matmul_readvariableop_resource_0"Z
*gru_while_gru_cell_readvariableop_resource,gru_while_gru_cell_readvariableop_resource_0"@
gru_while_gru_strided_slice_1gru_while_gru_strided_slice_1_0"1
gru_while_identitygru/while/Identity:output:0"5
gru_while_identity_1gru/while/Identity_1:output:0"5
gru_while_identity_2gru/while/Identity_2:output:0"5
gru_while_identity_3gru/while/Identity_3:output:0"5
gru_while_identity_4gru/while/Identity_4:output:0"Є
Ygru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor[gru_while_tensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :€€€€€€€€€†: : : : : 2T
(gru/while/gru_cell/MatMul/ReadVariableOp(gru/while/gru_cell/MatMul/ReadVariableOp2X
*gru/while/gru_cell/MatMul_1/ReadVariableOp*gru/while/gru_cell/MatMul_1/ReadVariableOp2F
!gru/while/gru_cell/ReadVariableOp!gru/while/gru_cell/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
: 
Ж
і
while_cond_25921382
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25921382___redundant_placeholder06
2while_while_cond_25921382___redundant_placeholder16
2while_while_cond_25921382___redundant_placeholder26
2while_while_cond_25921382___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :€€€€€€€€€†: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :.*
(
_output_shapes
:€€€€€€€€€†:

_output_shapes
: :

_output_shapes
:
Я;
Е
C__inference_gru_1_layer_call_and_return_conditional_losses_25919515

inputs%
gru_cell_1_25919439:`&
gru_cell_1_25919441:	†`%
gru_cell_1_25919443: `
identityИҐ"gru_cell_1/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permД
	transpose	Transposeinputstranspose/perm:output:0*
T0*5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€†   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2э
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*(
_output_shapes
:€€€€€€€€€†*
shrink_axis_mask2
strided_slice_2т
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_1_25919439gru_cell_1_25919441gru_cell_1_25919443*
Tin	
2*
Tout
2*
_collective_manager_ids
 *:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *Q
fLRJ
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_259193882$
"gru_cell_1/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЛ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_1_25919439gru_cell_1_25919441gru_cell_1_25919443*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
_stateful_parallelism( *
bodyR
while_body_25919451*
condR
while_cond_25919450*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimes
IdentityIdentitystrided_slice_3:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity{
NoOpNoOp#^gru_cell_1/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall2
whilewhile:] Y
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
 
_user_specified_nameinputs
ч

џ
+__inference_gru_cell_layer_call_fn_25922769

inputs
states_0
unknown:	а
	unknown_0:	а
	unknown_1:
†а
identity

identity_1ИҐStatefulPartitionedCall§
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*
_collective_manager_ids
 *<
_output_shapes*
(:€€€€€€€€€†:€€€€€€€€€†*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *O
fJRH
F__inference_gru_cell_layer_call_and_return_conditional_losses_259186792
StatefulPartitionedCall|
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

IdentityА

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*(
_output_shapes
:€€€€€€€€€†2

Identity_1h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-:€€€€€€€€€:€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:RN
(
_output_shapes
:€€€€€€€€€†
"
_user_specified_name
states/0
Д
і
while_cond_25919977
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_less_strided_slice_16
2while_while_cond_25919977___redundant_placeholder06
2while_while_cond_25919977___redundant_placeholder16
2while_while_cond_25919977___redundant_placeholder26
2while_while_cond_25919977___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
:
¬
є
(__inference_gru_1_layer_call_fn_25922636
inputs_0
unknown:`
	unknown_0:	†`
	unknown_1: `
identityИҐStatefulPartitionedCallВ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8В *L
fGRE
C__inference_gru_1_layer_call_and_return_conditional_losses_259195152
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*:
_input_shapes)
':€€€€€€€€€€€€€€€€€€†: : : 22
StatefulPartitionedCallStatefulPartitionedCall:_ [
5
_output_shapes#
!:€€€€€€€€€€€€€€€€€€†
"
_user_specified_name
inputs/0"®L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*∞
serving_defaultЬ
C
	gru_input6
serving_default_gru_input:0€€€€€€€€€9
dense0
StatefulPartitionedCall:0€€€€€€€€€tensorflow/serving/predict:ћ†
и
layer_with_weights-0
layer-0
layer-1
layer_with_weights-1
layer-2
layer_with_weights-2
layer-3
	optimizer
trainable_variables
	variables
regularization_losses
		keras_api


signatures
*o&call_and_return_all_conditional_losses
p_default_save_signature
q__call__"
_tf_keras_sequential
√
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
*r&call_and_return_all_conditional_losses
s__call__"
_tf_keras_rnn_layer
•
trainable_variables
	variables
regularization_losses
	keras_api
*t&call_and_return_all_conditional_losses
u__call__"
_tf_keras_layer
√
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
*v&call_and_return_all_conditional_losses
w__call__"
_tf_keras_rnn_layer
ї

kernel
bias
trainable_variables
	variables
regularization_losses
 	keras_api
*x&call_and_return_all_conditional_losses
y__call__"
_tf_keras_layer
£m_m`!ma"mb#mc$md%me&mfvgvh!vi"vj#vk$vl%vm&vn"
	optimizer
X
!0
"1
#2
$3
%4
&5
6
7"
trackable_list_wrapper
X
!0
"1
#2
$3
%4
&5
6
7"
trackable_list_wrapper
 "
trackable_list_wrapper
 

'layers
(layer_metrics
trainable_variables
)metrics
*non_trainable_variables
	variables
+layer_regularization_losses
regularization_losses
q__call__
p_default_save_signature
*o&call_and_return_all_conditional_losses
&o"call_and_return_conditional_losses"
_generic_user_object
,
zserving_default"
signature_map
—

!kernel
"recurrent_kernel
#bias
,trainable_variables
-	variables
.regularization_losses
/	keras_api
*{&call_and_return_all_conditional_losses
|__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
 "
trackable_list_wrapper
є

0layers
1layer_metrics
trainable_variables
2metrics
3non_trainable_variables
	variables
4layer_regularization_losses

5states
regularization_losses
s__call__
*r&call_and_return_all_conditional_losses
&r"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
≠

6layers
7layer_metrics
trainable_variables
8metrics
9non_trainable_variables
	variables
:layer_regularization_losses
regularization_losses
u__call__
*t&call_and_return_all_conditional_losses
&t"call_and_return_conditional_losses"
_generic_user_object
—

$kernel
%recurrent_kernel
&bias
;trainable_variables
<	variables
=regularization_losses
>	keras_api
*}&call_and_return_all_conditional_losses
~__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
 "
trackable_list_wrapper
є

?layers
@layer_metrics
trainable_variables
Ametrics
Bnon_trainable_variables
	variables
Clayer_regularization_losses

Dstates
regularization_losses
w__call__
*v&call_and_return_all_conditional_losses
&v"call_and_return_conditional_losses"
_generic_user_object
: 2dense/kernel
:2
dense/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
≠

Elayers
Flayer_metrics
trainable_variables
Gmetrics
Hnon_trainable_variables
	variables
Ilayer_regularization_losses
regularization_losses
y__call__
*x&call_and_return_all_conditional_losses
&x"call_and_return_conditional_losses"
_generic_user_object
&:$	а2gru/gru_cell/kernel
1:/
†а2gru/gru_cell/recurrent_kernel
$:"	а2gru/gru_cell/bias
*:(	†`2gru_1/gru_cell_1/kernel
3:1 `2!gru_1/gru_cell_1/recurrent_kernel
':%`2gru_1/gru_cell_1/bias
<
0
1
2
3"
trackable_list_wrapper
 "
trackable_dict_wrapper
.
J0
K1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
 "
trackable_list_wrapper
≠

Llayers
Mlayer_metrics
,trainable_variables
Nmetrics
Onon_trainable_variables
-	variables
Player_regularization_losses
.regularization_losses
|__call__
*{&call_and_return_all_conditional_losses
&{"call_and_return_conditional_losses"
_generic_user_object
'
0"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
 "
trackable_list_wrapper
≠

Qlayers
Rlayer_metrics
;trainable_variables
Smetrics
Tnon_trainable_variables
<	variables
Ulayer_regularization_losses
=regularization_losses
~__call__
*}&call_and_return_all_conditional_losses
&}"call_and_return_conditional_losses"
_generic_user_object
'
0"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
N
	Vtotal
	Wcount
X	variables
Y	keras_api"
_tf_keras_metric
^
	Ztotal
	[count
\
_fn_kwargs
]	variables
^	keras_api"
_tf_keras_metric
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
:  (2total
:  (2count
.
V0
W1"
trackable_list_wrapper
-
X	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
Z0
[1"
trackable_list_wrapper
-
]	variables"
_generic_user_object
: 2dense/kernel/m
:2dense/bias/m
&:$	а2gru/gru_cell/kernel/m
1:/
†а2gru/gru_cell/recurrent_kernel/m
$:"	а2gru/gru_cell/bias/m
*:(	†`2gru_1/gru_cell_1/kernel/m
3:1 `2#gru_1/gru_cell_1/recurrent_kernel/m
':%`2gru_1/gru_cell_1/bias/m
: 2dense/kernel/v
:2dense/bias/v
&:$	а2gru/gru_cell/kernel/v
1:/
†а2gru/gru_cell/recurrent_kernel/v
$:"	а2gru/gru_cell/bias/v
*:(	†`2gru_1/gru_cell_1/kernel/v
3:1 `2#gru_1/gru_cell_1/recurrent_kernel/v
':%`2gru_1/gru_cell_1/bias/v
о2л
H__inference_sequential_layer_call_and_return_conditional_losses_25920961
H__inference_sequential_layer_call_and_return_conditional_losses_25921277
H__inference_sequential_layer_call_and_return_conditional_losses_25920605
H__inference_sequential_layer_call_and_return_conditional_losses_25920629ј
Ј≤≥
FullArgSpec1
args)Ъ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsЪ
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
–BЌ
#__inference__wrapped_model_25918609	gru_input"Ш
С≤Н
FullArgSpec
argsЪ 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
В2€
-__inference_sequential_layer_call_fn_25920111
-__inference_sequential_layer_call_fn_25921298
-__inference_sequential_layer_call_fn_25921319
-__inference_sequential_layer_call_fn_25920581ј
Ј≤≥
FullArgSpec1
args)Ъ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsЪ
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
з2д
A__inference_gru_layer_call_and_return_conditional_losses_25921472
A__inference_gru_layer_call_and_return_conditional_losses_25921625
A__inference_gru_layer_call_and_return_conditional_losses_25921778
A__inference_gru_layer_call_and_return_conditional_losses_25921931’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
ы2ш
&__inference_gru_layer_call_fn_25921942
&__inference_gru_layer_call_fn_25921953
&__inference_gru_layer_call_fn_25921964
&__inference_gru_layer_call_fn_25921975’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
»2≈
E__inference_dropout_layer_call_and_return_conditional_losses_25921980
E__inference_dropout_layer_call_and_return_conditional_losses_25921992і
Ђ≤І
FullArgSpec)
args!Ъ
jself
jinputs

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
Т2П
*__inference_dropout_layer_call_fn_25921997
*__inference_dropout_layer_call_fn_25922002і
Ђ≤І
FullArgSpec)
args!Ъ
jself
jinputs

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
п2м
C__inference_gru_1_layer_call_and_return_conditional_losses_25922155
C__inference_gru_1_layer_call_and_return_conditional_losses_25922308
C__inference_gru_1_layer_call_and_return_conditional_losses_25922461
C__inference_gru_1_layer_call_and_return_conditional_losses_25922614’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
Г2А
(__inference_gru_1_layer_call_fn_25922625
(__inference_gru_1_layer_call_fn_25922636
(__inference_gru_1_layer_call_fn_25922647
(__inference_gru_1_layer_call_fn_25922658’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
н2к
C__inference_dense_layer_call_and_return_conditional_losses_25922668Ґ
Щ≤Х
FullArgSpec
argsЪ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
“2ѕ
(__inference_dense_layer_call_fn_25922677Ґ
Щ≤Х
FullArgSpec
argsЪ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
ѕBћ
&__inference_signature_wrapper_25920652	gru_input"Ф
Н≤Й
FullArgSpec
argsЪ 
varargs
 
varkwjkwargs
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
‘2—
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922716
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922755Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
Ю2Ы
+__inference_gru_cell_layer_call_fn_25922769
+__inference_gru_cell_layer_call_fn_25922783Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
Ў2’
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922822
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922861Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
Ґ2Я
-__inference_gru_cell_1_layer_call_fn_25922875
-__inference_gru_cell_1_layer_call_fn_25922889Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 Ш
#__inference__wrapped_model_25918609q#!"&$%6Ґ3
,Ґ)
'К$
	gru_input€€€€€€€€€
™ "-™*
(
denseК
dense€€€€€€€€€£
C__inference_dense_layer_call_and_return_conditional_losses_25922668\/Ґ,
%Ґ"
 К
inputs€€€€€€€€€ 
™ "%Ґ"
К
0€€€€€€€€€
Ъ {
(__inference_dense_layer_call_fn_25922677O/Ґ,
%Ґ"
 К
inputs€€€€€€€€€ 
™ "К€€€€€€€€€ѓ
E__inference_dropout_layer_call_and_return_conditional_losses_25921980f8Ґ5
.Ґ+
%К"
inputs€€€€€€€€€†
p 
™ "*Ґ'
 К
0€€€€€€€€€†
Ъ ѓ
E__inference_dropout_layer_call_and_return_conditional_losses_25921992f8Ґ5
.Ґ+
%К"
inputs€€€€€€€€€†
p
™ "*Ґ'
 К
0€€€€€€€€€†
Ъ З
*__inference_dropout_layer_call_fn_25921997Y8Ґ5
.Ґ+
%К"
inputs€€€€€€€€€†
p 
™ "К€€€€€€€€€†З
*__inference_dropout_layer_call_fn_25922002Y8Ґ5
.Ґ+
%К"
inputs€€€€€€€€€†
p
™ "К€€€€€€€€€†≈
C__inference_gru_1_layer_call_and_return_conditional_losses_25922155~&$%PҐM
FҐC
5Ъ2
0К-
inputs/0€€€€€€€€€€€€€€€€€€†

 
p 

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ ≈
C__inference_gru_1_layer_call_and_return_conditional_losses_25922308~&$%PҐM
FҐC
5Ъ2
0К-
inputs/0€€€€€€€€€€€€€€€€€€†

 
p

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ µ
C__inference_gru_1_layer_call_and_return_conditional_losses_25922461n&$%@Ґ=
6Ґ3
%К"
inputs€€€€€€€€€†

 
p 

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ µ
C__inference_gru_1_layer_call_and_return_conditional_losses_25922614n&$%@Ґ=
6Ґ3
%К"
inputs€€€€€€€€€†

 
p

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ Э
(__inference_gru_1_layer_call_fn_25922625q&$%PҐM
FҐC
5Ъ2
0К-
inputs/0€€€€€€€€€€€€€€€€€€†

 
p 

 
™ "К€€€€€€€€€ Э
(__inference_gru_1_layer_call_fn_25922636q&$%PҐM
FҐC
5Ъ2
0К-
inputs/0€€€€€€€€€€€€€€€€€€†

 
p

 
™ "К€€€€€€€€€ Н
(__inference_gru_1_layer_call_fn_25922647a&$%@Ґ=
6Ґ3
%К"
inputs€€€€€€€€€†

 
p 

 
™ "К€€€€€€€€€ Н
(__inference_gru_1_layer_call_fn_25922658a&$%@Ґ=
6Ґ3
%К"
inputs€€€€€€€€€†

 
p

 
™ "К€€€€€€€€€ Е
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922822Є&$%]ҐZ
SҐP
!К
inputs€€€€€€€€€†
'Ґ$
"К
states/0€€€€€€€€€ 
p 
™ "RҐO
HҐE
К
0/0€€€€€€€€€ 
$Ъ!
К
0/1/0€€€€€€€€€ 
Ъ Е
H__inference_gru_cell_1_layer_call_and_return_conditional_losses_25922861Є&$%]ҐZ
SҐP
!К
inputs€€€€€€€€€†
'Ґ$
"К
states/0€€€€€€€€€ 
p
™ "RҐO
HҐE
К
0/0€€€€€€€€€ 
$Ъ!
К
0/1/0€€€€€€€€€ 
Ъ №
-__inference_gru_cell_1_layer_call_fn_25922875™&$%]ҐZ
SҐP
!К
inputs€€€€€€€€€†
'Ґ$
"К
states/0€€€€€€€€€ 
p 
™ "DҐA
К
0€€€€€€€€€ 
"Ъ
К
1/0€€€€€€€€€ №
-__inference_gru_cell_1_layer_call_fn_25922889™&$%]ҐZ
SҐP
!К
inputs€€€€€€€€€†
'Ґ$
"К
states/0€€€€€€€€€ 
p
™ "DҐA
К
0€€€€€€€€€ 
"Ъ
К
1/0€€€€€€€€€ Е
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922716Ї#!"]ҐZ
SҐP
 К
inputs€€€€€€€€€
(Ґ%
#К 
states/0€€€€€€€€€†
p 
™ "TҐQ
JҐG
К
0/0€€€€€€€€€†
%Ъ"
 К
0/1/0€€€€€€€€€†
Ъ Е
F__inference_gru_cell_layer_call_and_return_conditional_losses_25922755Ї#!"]ҐZ
SҐP
 К
inputs€€€€€€€€€
(Ґ%
#К 
states/0€€€€€€€€€†
p
™ "TҐQ
JҐG
К
0/0€€€€€€€€€†
%Ъ"
 К
0/1/0€€€€€€€€€†
Ъ №
+__inference_gru_cell_layer_call_fn_25922769ђ#!"]ҐZ
SҐP
 К
inputs€€€€€€€€€
(Ґ%
#К 
states/0€€€€€€€€€†
p 
™ "FҐC
К
0€€€€€€€€€†
#Ъ 
К
1/0€€€€€€€€€†№
+__inference_gru_cell_layer_call_fn_25922783ђ#!"]ҐZ
SҐP
 К
inputs€€€€€€€€€
(Ґ%
#К 
states/0€€€€€€€€€†
p
™ "FҐC
К
0€€€€€€€€€†
#Ъ 
К
1/0€€€€€€€€€†—
A__inference_gru_layer_call_and_return_conditional_losses_25921472Л#!"OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p 

 
™ "3Ґ0
)К&
0€€€€€€€€€€€€€€€€€€†
Ъ —
A__inference_gru_layer_call_and_return_conditional_losses_25921625Л#!"OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p

 
™ "3Ґ0
)К&
0€€€€€€€€€€€€€€€€€€†
Ъ Ј
A__inference_gru_layer_call_and_return_conditional_losses_25921778r#!"?Ґ<
5Ґ2
$К!
inputs€€€€€€€€€

 
p 

 
™ "*Ґ'
 К
0€€€€€€€€€†
Ъ Ј
A__inference_gru_layer_call_and_return_conditional_losses_25921931r#!"?Ґ<
5Ґ2
$К!
inputs€€€€€€€€€

 
p

 
™ "*Ґ'
 К
0€€€€€€€€€†
Ъ ®
&__inference_gru_layer_call_fn_25921942~#!"OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p 

 
™ "&К#€€€€€€€€€€€€€€€€€€†®
&__inference_gru_layer_call_fn_25921953~#!"OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p

 
™ "&К#€€€€€€€€€€€€€€€€€€†П
&__inference_gru_layer_call_fn_25921964e#!"?Ґ<
5Ґ2
$К!
inputs€€€€€€€€€

 
p 

 
™ "К€€€€€€€€€†П
&__inference_gru_layer_call_fn_25921975e#!"?Ґ<
5Ґ2
$К!
inputs€€€€€€€€€

 
p

 
™ "К€€€€€€€€€†љ
H__inference_sequential_layer_call_and_return_conditional_losses_25920605q#!"&$%>Ґ;
4Ґ1
'К$
	gru_input€€€€€€€€€
p 

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ љ
H__inference_sequential_layer_call_and_return_conditional_losses_25920629q#!"&$%>Ґ;
4Ґ1
'К$
	gru_input€€€€€€€€€
p

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ Ї
H__inference_sequential_layer_call_and_return_conditional_losses_25920961n#!"&$%;Ґ8
1Ґ.
$К!
inputs€€€€€€€€€
p 

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ Ї
H__inference_sequential_layer_call_and_return_conditional_losses_25921277n#!"&$%;Ґ8
1Ґ.
$К!
inputs€€€€€€€€€
p

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ Х
-__inference_sequential_layer_call_fn_25920111d#!"&$%>Ґ;
4Ґ1
'К$
	gru_input€€€€€€€€€
p 

 
™ "К€€€€€€€€€Х
-__inference_sequential_layer_call_fn_25920581d#!"&$%>Ґ;
4Ґ1
'К$
	gru_input€€€€€€€€€
p

 
™ "К€€€€€€€€€Т
-__inference_sequential_layer_call_fn_25921298a#!"&$%;Ґ8
1Ґ.
$К!
inputs€€€€€€€€€
p 

 
™ "К€€€€€€€€€Т
-__inference_sequential_layer_call_fn_25921319a#!"&$%;Ґ8
1Ґ.
$К!
inputs€€€€€€€€€
p

 
™ "К€€€€€€€€€®
&__inference_signature_wrapper_25920652~#!"&$%CҐ@
Ґ 
9™6
4
	gru_input'К$
	gru_input€€€€€€€€€"-™*
(
denseК
dense€€€€€€€€€