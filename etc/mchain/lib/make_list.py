

def make_list(dims):
	arr = [0 for j in range(dims[0])]
	return make_list2(dims[1:], arr )


def make_list2(dims, arr):
	if len(dims) == 1:
		for i in range(len(arr)):
			arr[i]= [0 for j in range(dims[0])]
		return arr

	else:
		for i in range(len(arr)):
			arr[i]= [0 for j in range(dims[0])]
			make_list2(dims[1:], arr[i])
		return arr


def add_to_counter(value, arr):
	co = arr

	for i in value[:-1]:
		co = co[i]

	co[value[-1]] +=1