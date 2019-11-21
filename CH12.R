#
ch12_1<-function(deg,unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>200){
    net.price<-net.price*1.15
  }
  round(net.price)
}
ch12_1(100)
ch12_1(150)
ch12_1(250)

#
ch12_2<-function(deg, unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>200)net.price<-net.price*1.15
  round(net.price)
}
ch12_2(150)
ch12_2(250)

#
ch12_3<-function(deg, unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>200)
    net.price<-net.price*1.15
  round(net.price)
}
ch12_3(150)
ch12_3(deg=250)

#
ch12_4<-function(deg, unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>100)
    net.price<-net.price*1.15
  else
    net.price<-net.price*0.85
  round(net.price)
}
ch12_4(deg=80)
ch12_4(deg=200)

#
ch12_5<-function(deg,unitPrice=50)
{
  net.price<-deg*unitPrice
  adjustment<-if(deg>100)1.15 else 0.85
  total.price<-net.price*adjustment
  round(total.price)
}
ch12_5(deg=80)
ch12_5(deg=200)

#
ch12_6<-function(deg,unitPrice=50)
{
  net.price<-deg*unitPrice
  total.price<-net.price*if(deg>100)1.15 else 0.85
  round(total.price)
}
ch12_6(deg=80)
ch12_6(deg=200)

#
ch12_7<-function(deg,unitPrice=50)
{
  if(deg>120)
    net.price<-deg*unitPrice*1.15
  else if(deg<80)
    net.price<-deg*unitPrice*0.85
  else
    net.price<-deg-unitPrice
  round(net.price)
}

ch12_7(deg=70)
ch12_7(deg=100)
ch12_7(deg=150)

#
ch12_8<-function(deg,poor=FALSE, unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>100)
    net.price<-net.price*1.15
  else{
    net.price<-net.price*0.85
    if(poor==TRUE)
    net.price<-net.price*0.7
}
      round(net.price)
}

ch12_8(deg=80)

#
ch12_9<-function(deg,poor=FALSE, unitPrice=50)
{
  net.price<-deg*unitPrice
  if(deg>100)
    net.price<-net.price*1.15
  else{
    net.price<-net.price*0.85
    if(poor)
      net.price<-net.price*0.7
  }
  round(net.price)
}

ch12_9(deg=80)

#
ch12_11<-function(deg,untPrice=50)
{
  net.price<-deg*unitPrice
  net.price=net.price*ifelse((deg>100),1.15,0.85)
  round(net.price)
}
ch12_11(c(80,200))

#
ch12_12<-function(deg,poor=FALSE,unitPrice=50)
{
  net.price<-deg*unitPrice
  net.price=net.price*ifelse(deg>100,1.15,0.85)
  net.price=net.price*ifelse(deg<=100& poor,0.7,1)
  round(net.price)
}
deginfo<-c(80,80,200,200)
poorinfo<-c(TRUE,FALSE,TRUE,FALSE)
ch12_12(deginfo,poorinfo)

ch12_13<-function(deg, unitPrice=50)
{
  if(deg>120) index<-1
  if(deg<=120&deg>=80)index<-2
  if(deg<80)index<-3
  seitch(index,
         net.price<-deg*unitPrice*1.15,
         net.price<-deg*unitPrice,
         net.price<-deg*untPrice*0.85)
  round(net.price)
}
ch12_13(deg=70)
ch12_13(deg=100)
ch12_13(deg=150)

#
ch12_14<-function(type)
{
  switch(type,iphone="Apple",
         TV="Sony",
         PC="Dell")
}
ch12_14("TV")
ch12_14("iphone")
ch12_14("PC")

#
ch12_16<-function(n)
{
  sumx<-0
  for(i in n)sumx<-sumx+i
  print(sumx)
}
ch12_16(1:10)
ch12_16(1:100)

#
ch12_17<-function(n)
{
  counter<-0
  for(i in n)
  {
    if(i=="North Central")
      counter<-counter+1
  }
  print(counter)
}
ch12_17(state.region)

#
ch12_18<-function(n)
{
  p_sum<-0
  for(i in state.x77[,"Population"])
    p_sum<-p_sum+i
  print(p_sum)
}

ch12_18(state.x77[,"Population"])

#
ch12_19<-function(deg,customer, unitPrice=50)
{
  listprice<-deg*unitPrice *
    ifelse(deg>150,0.8,1)
  adj<-numeric(0)
  for(i in customer){
    adj<-c(adj,switch(i,goverment=0.8, company=1.2,1))
  }
  finalprice<-listprice*adj
  round(finalprice)
}

#deginfo
#custinfo
#ch12_19(deginfo,custinfo)

#
ch12_21<-function(x)
{
  sumx<-0
  while(x>=0)
  {
    sumx<-sumx+x
    x<-x-1
  }
  return(sumx)
}

ch12_21(10)
ch12_21(100)

#
ch12_22<-function(x)
{
  sumx<-0
  repeat
  {
    sumx<-sumx+x
    if(x==0)break
    x<-x-1
  }
  return(sumx)
}
ch12_22(10)
ch12_22(100)









