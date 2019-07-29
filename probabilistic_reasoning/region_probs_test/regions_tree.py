maxCodes = [2,2]

def calculate_regions(*args):
  if 1 >= args[0].var_1:
    if args[0].var_2 >= args[1].var_1:
        return [0,0]
    
    if args[1].var_1 > args[0].var_2:
      return [0,1]
  
  if args[0].var_1 > 1:
    if args[1].var_1 > args[0].var_1:
        return [1,0]
    
    if args[0].var_1 >= args[1].var_1:
      return [1,1]