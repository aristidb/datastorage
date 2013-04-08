CXX=g++-4.8
CXXFLAGS=-std=c++11 -Wall -Wextra -O3

.PHONY: all clean

all: rh

rh: rollinghash.cpp
	$(CXX) $(CXXFLAGS) -o rh rollinghash.cpp
