stdatm <- function(Z,param="all",k=1){
  
  # Based on stdatm.m from:
  # http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html
  # k=0 for metric, 1 for std
  #     Z  - input altitude, in feet or meters (depending on k)
  #
  #     output:
  #                      units: metric        English
  #     T  - temp.               deg K         deg R
  #     P  - pressure            N/m^2         lb/ft^2
  #     R  - density (rho)       Kg/m^3        slug/ft^3
  #     A  - speed of sound      m/sec         ft/sec
  #     MU - viscosity           Kg/(m sec)    slug/<ft sec)
  #     
  #     TS - t/t at sea level
  #     RR - rho/rho at sea level
  #     PP - p/p at sea level
  #
  #     RM - Reynolds number per Mach per unit of length
  #     QM - dynamic pressure/Mach^2
 
         
         
  
  KK = 0;
  K = 34.163195;
  C1 = 3.048e-4;
  T = 1;
  PP = 0;
  
  if (k==0){
    TL = 288.15;
    PL = 101325;
    RL = 1.225;
    C1 = 0.001;
    AL = 340.294;
    ML = 1.7894e-5;
    BT = 1.458e-6;
  }
  else{
    TL = 518.67;
    PL = 2116.22;
    RL = 0.0023769;
    AL = 1116.45;
    ML = 3.7373e-7;
    BT = 3.0450963e-8;
  }
  
  H = C1*Z/(1 + C1*Z/6356.766);
  
  if (H<11){
    T = 288.15 - 6.5*H;
    PP = (288.15/T)^(-K/6.5);
  }
  else{
    if (H<20){
      T = 216.65;
      PP = 0.22336*exp(-K*(H-11)/216.65);
    }
    else{
      if  (H<32){
        T = 216.65 + (H-20);
        PP = 0.054032*(216.65/T)^K;
      }
      else{
        if  (H<47){
          T = 228.65 + 2.8*(H-32);
          PP = 0.0085666*(228.65/T)^(K/2.8);
        }
        else{
          if  (H<51){
            T = 270.65;
            PP = 0.0010945*exp(-K*(H-47)/270.65);
          }
          else{
            if  (H<71){
              T = 270.65 - 2.8*(H-51);
              PP = 0.00066063*(270.65/T)^(-K/2.8);
            }
            else{
              if  (H<84.852){
                T = 214.65 - 2*(H-71);
                PP = 3.9046e-5*(214.65/T)^(-K/2);
              }
              else{
                
                # chosen altitude too high
                KK = 1;
              }
            }
          }
        }
      }
    }
  }
  
  M1 = sqrt(1.4*287*T);
  RR = PP/(T/288.15);
  MU = BT*T^1.5/(T+110.4);
  TS = T/288.15;
  A = AL*sqrt(TS);
  T = TL*TS;
  R = RL*RR;
  P = PL*PP;
  RM = R*A/MU;
  QM = 0.7*P;
  
  
  switch(param,
         all={return(list(T=T,P=P,R=R,A=A,MU=MU,TS=TS,RR=RR,PP=PP,RM=RM,QM=QM))},
         T={return(T)},
         P={return(P)},
         R={return(R)},
         A={return(A)},
         MU={return(MU)},
         TS={return(TS)},
         RR={return(RR)},
         PP={return(PP)},
         RM={return(RM)},
         QM={return(QM)})
  
}