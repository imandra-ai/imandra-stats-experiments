def sample(arg):
   n = 0
   v0 = 0
   v1 = 0
   
   d0 = arg.a00*arg.a11 - arg.a10*arg.a01
   print("d0 =",d0)
   if d0 != 0:
       x0 = (arg.b0*arg.a11 - arg.b0*arg.a01) / d0
       x1 = (arg.a00*arg.b1 - arg.a10*arg.b1) / d0
       print("  - d0",x0,x1)
       c2 = arg.a20*x0 + arg.a21*x1 <= arg.b2
       c3 = arg.a30*x0 + arg.a31*x1 <= arg.b3
       if c2 and c3:
           n += 1
           v0 += x0
           v1 += x1
   
   d1 = arg.a00*arg.a21 - arg.a20*arg.a01
   print("d1 =",d1)
   if d1 != 0:
       x0 = (arg.b0*arg.a21 - arg.b0*arg.a01) / d1
       x1 = (arg.a00*arg.b1 - arg.a20*arg.b1) / d1
       print("  - d1",x0,x1)
       c1 = arg.a10*x0 + arg.a11*x1 <= arg.b1
       c3 = arg.a30*x0 + arg.a31*x1 <= arg.b3
       if c1 and c3:
           n += 1
           v0 += x0
           v1 += x1
   
   d2 = arg.a00*arg.a31 - arg.a30*arg.a01
   print("d2 =",d2)
   if d2 != 0:
       x0 = (arg.b0*arg.a31 - arg.b0*arg.a01) / d2
       x1 = (arg.a00*arg.b1 - arg.a30*arg.b1) / d2
       print("  - d2",x0,x1)
       c1 = arg.a10*x0 + arg.a11*x1 <= arg.b1
       c2 = arg.a20*x0 + arg.a21*x1 <= arg.b2
       if c1 and c2:
           n += 1
           v0 += x0
           v1 += x1
   
   d3 = arg.a10*arg.a21 - arg.a20*arg.a11
   if d3 != 0:
       x0 = (arg.b0*arg.a21 - arg.b0*arg.a11) / d3
       x1 = (arg.a10*arg.b1 - arg.a20*arg.b1) / d3
       print(x0,x1)
       c0 = arg.a00*x0 + arg.a01*x1 <= arg.b0
       c3 = arg.a30*x0 + arg.a31*x1 <= arg.b3
       if c0 and c3:
           n += 1
           v0 += x0
           v1 += x1
   
   d4 = arg.a10*arg.a31 - arg.a30*arg.a11
   if d4 != 0:
       x0 = (arg.b0*arg.a31 - arg.b0*arg.a11) / d4
       x1 = (arg.a10*arg.b1 - arg.a30*arg.b1) / d4
       print(x0,x1)
       c0 = arg.a00*x0 + arg.a01*x1 <= arg.b0
       c2 = arg.a20*x0 + arg.a21*x1 <= arg.b2
       if c0 and c2:
           n += 1
           v0 += x0
           v1 += x1
   
   d5 = arg.a20*arg.a31 - arg.a30*arg.a21
   if d5 != 0:
       x0 = (arg.b0*arg.a31 - arg.b0*arg.a21) / d5
       x1 = (arg.a20*arg.b1 - arg.a30*arg.b1) / d5
       print(x0,x1)
       c0 = arg.a00*x0 + arg.a01*x1 <= arg.b0
       c1 = arg.a10*x0 + arg.a11*x1 <= arg.b1
       if c0 and c1:
           n += 1
           v0 += x0
           v1 += x1
   
   print("n =", n)
   v0 /= n
   v1 /= n
   return (v0,v1)