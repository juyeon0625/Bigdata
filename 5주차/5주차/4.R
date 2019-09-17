sim = function (nreps) {
  
  commdata = list() #새 위원회에 대한 정보를 입력할 곳
  commdata$countabsamecomm = 0
  
  for (rep in 1:nreps) {
    commdata$whosleft = 1:20 #위원회를 뽑을 대상 
    commdata$numabchosen = 0 #A, B중 이미 뽑힌 대상
    
    commdata = choosecomm (commdata, 5) 
    if (commdata$numabchosen > 0) next
    
    commdata = choosecomm(commdata, 4)
    if (commdata$numabchosen > 0) next
    
    commdata = choosecomm(commdata, 3)
  }
  print(commdata$countabsamecomm/nreps)
}


choosecomm = function(comdat, comsize)
{
  committee = sample(comdat$whosleft, comsize)
  comdat$numabchosen = length(intersect(1:2, committee))

  if (comdat$numabchosen == 2)
    comdat$countabsamecomm = comdat$countabsamecomm + 1
  
  comdat$whosleft = setdiff(comdat$whosleft, committee)
    
  return(comdat)
}

for(i in 1:10) sim(10)
for(i in 1:10) sim(100)

