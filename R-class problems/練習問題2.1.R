%練習問題2.1

2.1.1
%*****************************
?append
%description : It adds elements to a vector.
%Usage: append(x,values,after = length(x))
append(1:5,0:1,after = 3) 
% it adds 0 to 1 after the 3rd element of the vector that contains 1 to 5 as its elements, atlast element number increases from 5 to 7
% 1 2 3 0 1 4 5

2.1.2
%*****************************
?rep
% description: this replicates the values in x. 
rep(1:4,2) 
%it replicates 1 to 4 twice.
% 1 2 3 4 1 2 3 4
rep(1:4,each = 2) 
%it replicates each of 1 to 4 twice
1 1 2 2 3 3 4 4
rep(1:4,c(2,2,2,2))
%same result gets produced
rep(1:4,c(2,1,2,1))
% 1 1 2 3 3 4
rep(1:4,each = 2, len = 4)
% 1 1 2 2 produces first 4 elements only
rep(1:4,each = 3,times = 3)
%1 1 1 2 2 2 3 3 3 4 4 4 1 1 1 2 2 2 3 3 3 4 4 4 1 1 1 2 2 2 3 3 3 4 4 4
rep(1,40*(1-.8))
%1 1 1 1 1 1 1
fred<- list(happy = 1:10,name = "squash")
rep(fred,5)
%replicates the fred list five times
x<-.leap.seconds[1:3]
rep(x,2)
rep(as.POSIXlt(x),rep(2,3))

x <- factor(LETTERS[1:4]); names(x) <- letters[1:4]
x

2.1.3
%*****************************
?seq
%description:it generates the regular series
%seq(from,to)
%seq(from,to,by =)
%seq(from,to,lengh.out =)
%seq(along.with =)
%seq(from)
%seq(length.out =)
%Examples
seq(0, 1, length.out=11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by=0.05)
seq(17) # same as 1:17, or even better seq_len(17)

2.1.4
%*****************************
?which.min
%description: determines the index of the minimum element of a vector
x <- c(1:4,0:5,11)
which.min(x)
% x =  1  2  3  4  0  1  2  3  4  5 11
%index = 5
