import json

def recursive_sum(dict_obj, beaware_of_red=False):
	if not dict_obj:
		return 0

	if isinstance(dict_obj, list):
		return sum([recursive_sum(i, beaware_of_red=beaware_of_red) for i in dict_obj])

	if isinstance(dict_obj, dict):
		keys = dict_obj.keys()
		possible_reds = list(keys)
		values_one_step = [dict_obj.get(k) for k in keys]
		possible_reds.extend(values_one_step)
		if beaware_of_red and "red" in possible_reds:
			return 0
		return sum([recursive_sum(dict_obj.get(k), beaware_of_red=beaware_of_red) for k in keys])

	if isinstance(dict_obj, int):
		return dict_obj

	return 0

with open('input.json', 'r') as f:
	data = f.read()
	decoded = json.loads(data)
	print(recursive_sum(decoded))
	print(recursive_sum(decoded, beaware_of_red=True))

