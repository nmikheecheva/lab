import numpy as np

#task 1.7.7
import urllib
from urllib import request
#fname = input("http://stepic.org/media/attachments/lesson/16462/boston_houses.csv")  # read file name from stdin
f = urllib.request.urlopen('https://stepic.org/media/attachments/lesson/16462/boston_houses.csv')  # open file from URL

'''
data = np.loadtxt(f, delimiter=',', skiprows=1)  # load data to work with
#data = np.matrix('10 60 60; 7 50 50; 12 75 75')
x2 = data[:,1:]
y = data[:,0]
z = np.ones_like(y)
x = np.hstack((z,x2))
e = ((np.linalg.inv((x.T).dot(x))).dot(x.T)).dot(y)
#e3 = " ".join(map(str,e.A1))
#print (e3)
'''

'''
#task 1.7.5
x = np.matrix('1 60; 1 50; 1 75')
y = np.matrix('10; 7; 12')
e1 = x.T
e2 = e1.dot(x)
e3 = np.linalg.inv(e2)
e4 = e3.dot(x.T)
e5 = e4.dot(y)
print(e5)
#or just
e = ((np.linalg.inv((x.T).dot(x))).dot(x.T)).dot(y)
print (e)
'''

'''
#task 1.6.4
from urllib.request import urlopen
filename = input() 
#https://stepic.org/media/attachments/lesson/16462/boston_houses.csv
f = urlopen(filename)
v = np.loadtxt(f, skiprows=1, delimiter=",")
print (v.mean(axis = 0))
'''

'''
#task 1.6.3
x_shape = tuple(map(int, input().split()))
X = np.fromiter(map(int, input().split()), np.int).reshape(x_shape)
y_shape = tuple(map(int, input().split()))
Y = np.fromiter(map(int, input().split()), np.int).reshape(y_shape)

if (x_shape[1] != y_shape[1]):
    print("matrix shapes do not match")
else:
    C = X.dot(Y.T)
    print (C)

'''


'''
#task 1.6.1-2
a = np.eye(3, 4, k=0)
b = np.eye(3, 4, k=1)
c = 2*a+ b
c = c.flatten()
c = c.reshape(12,1)
print(c)
'''
