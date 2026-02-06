
alpha_W<-5
RRS<-0.1
alpha_H<-5*RRS
Rmax=5000
A<-1
S_W<-c(rep(0,10),seq(1,10000,length.out=10))
S_H=c(seq(1,10000,length.out=10),rep(0,10))

dat<-tibble(S_W=S_W,S_H=S_H)

R_func<-function(alpha_W,S_W,alpha_H,S_H,A,Rmax, type){ #type = 
  #R = (alpha_W*S_W + alpha_H*S_H) / (1 + (alpha_W*S_W + alpha_H*S_H)/(A*Rmax));
  R = (alpha_W*S_W + alpha_H*S_H) * exp(-(alpha_W*S_W + alpha_H*S_H)/(exp(1)*A*Rmax));
  return(R)
}

R_func(
  alpha_W=alpha_W,
  alpha_H=alpha_H,
  A=A,
  Rmax=Rmax,
  S_W=S_W,
  S_H=S_H
)

       


dat%>%
  mutate(
    pHOS= S_H/(S_H+S_W),
    S=S_H+S_W,
    R =R_func(
      alpha_W=alpha_W,
      alpha_H=alpha_H,
      A=A,
      Rmax=Rmax,
      S_W=S_W,
      S_H=S_H
    )
  )%>%
  pivot_wider(names_from = pHOS,values_from = R,id_cols=c("S"))%>%
  mutate(RRS=`1`/`0`)
