library(shiny)
source("helpers.R")

shinyServer(function(input, output) {
  
  output$flightEnvelope <- renderPlot({

    #Conversions
    fps_per_knot = 1.6878;
    g = 32.2;
    
    #Data Setup
    h=input$h
    W=input$W
    CLmax=input$CLmax
    Sref=input$Sref
    DesignLoadFactor=input$DesignLoadFactor
    Vne=input$Vne
    Cla=input$Cla*180/pi
    mean_geom_chord=input$mean_geom_chord
    
    
    Vpoints = 100;
    Vbuffer = 0.1*Vne;   #kts
    Nbuffer = 0.5;  #g
    
    if(min(DesignLoadFactor>0)){
      #   warning('Minimum Design Load Factor should be 0 or lower')
    }
    
    speed_kts = seq(0,Vne,length=Vpoints);    #kts
    
    Vne_fps = Vne * fps_per_knot;           #ft/s
    speed_fps = seq(0,Vne_fps,length=Vpoints);#ft/s
    
    #Gust Line Setup
    #Defined by FAR part 25 for(20,000ft and below
    Vg = c(66,50,25);    #ft/s [VB VC VD] gust magnitude
    
    #Plot Setup
    #   figure(1)
    #   close 1;
    #   figure(1)
    #   xlim([0,Vne+Vbuffer])
    #   ylim([min(min(DesignLoadFactor)-Nbuffer,-1),max(DesignLoadFactor)+Nbuffer])
    #   grid on
    #   hold on
    #   plot([0,Vne+Vbuffer],[0 0],'k--','LineWidth',1.5)
    plot(c(0,Vne+Vbuffer),
         c(0,0),
         xlim=c(0,Vne+Vbuffer),
         ylim=c(min(min(DesignLoadFactor)-Nbuffer,-1),max(DesignLoadFactor)+Nbuffer),
         type="l",
         col="black",
         lty=2,
         lwd=2,
         xlab="Indicated Airspeed (kts)",
         ylab="Load Factor (g)",
         main="Flight Envelope (V-n Diagram)")
    grid()
    #   xlabel('Indicated Airspeed (kts)','FontSize',14,'FontWeight','b')
    #   ylabel('Load Factor (g)','FontSize',14,'FontWeight','b')
    #   title('Flight Envelope (V-n Diagram)','FontSize',16,'FontWeight','b')
    
    #Output Parameter Setup
    Vstall_pos=NaN;
    Va_pos=NaN;
    Vstall_neg=NaN;
    Va_neg=NaN;
    
    n_gust_pos=matrix(0,length(Vg),Vpoints)
    n_gust_neg=matrix(0,length(Vg),Vpoints)
    
    
    #Stall line calculations
    for(alt in h){
      #plot one line for(each altitude requested
      
      #   [~,rho,~,~,~,~,density_ratio] = stdatm(alt,1); 
      rho=stdatm(alt,"R",1)#density slug/ft^3
      density_ratio=stdatm(alt,"RR",1)
      #convert IAS to TAS
      speed_fps_true = sqrt(speed_fps^2/density_ratio);
      
      for(weight in W){
        #plot one line for(each Weight configuration
        
        #calculate positive stall line
        n_pos = max(CLmax) * Sref * rho * speed_fps_true^2 / (2 * weight);
        
        #calculate negative stall line
        if(length(CLmax)>1){
          n_neg = min(CLmax) * Sref * rho * speed_fps_true^2 / (2 * weight);
        }
        else{
          n_neg = rep(0,length(speed_fps));
        }
        
        #calculate gust lines
        #comes from: http://adg.stanford.edu/aa241/structures/vn.html
        for(j in 1:length(Vg)){
          mu = 2 * weight / (Sref * rho * mean_geom_chord * Cla * g);
          Kg = 0.88 * mu / (5.3 + mu);
          n_gust_pos[j,]=1+(Kg * Cla * Vg[j] * speed_kts) / (498 * weight / Sref);
          n_gust_neg[j,]=1-(Kg * Cla * Vg[j] * speed_kts) / (498 * weight / Sref);
        }
        
        #Find Stall Speed
        Vstall_pos = c(Vstall_pos,approx(n_pos,speed_kts,1)[["y"]]);
        #   approx(seq(1,5),seq(11,15),1.5)[["y"]]
        Vstall_neg = c(Vstall_neg,approx(n_neg,speed_kts,-1)[["y"]]);
        
        #truncate stall lines to limit loads
        n_pos[n_pos>max(DesignLoadFactor)]=max(DesignLoadFactor);
        n_neg[n_neg<min(DesignLoadFactor)]=min(c(DesignLoadFactor,0));
        
        #Find Maneuver Speed
        # Va_pos = [Va_pos speed_kts(max(find(n_pos<max(DesignLoadFactor))))];
        # Va_neg = [Va_neg speed_kts(max(find(n_neg>min(DesignLoadFactor))))];
        Va_pos=c(Va_pos,max(speed_kts[n_pos<max(DesignLoadFactor)]))
        Va_neg=c(Va_neg,max(speed_kts[n_neg>min(DesignLoadFactor)])) #that first min maybe should be max
        
        #plot the envelope lines
        #   figure(1)
        #   hold on
        #   plot(speed_kts,n_pos,'LineWidth',2);
        lines(speed_kts,
              n_pos,
              col="blue",
              type="l",
              lwd=2)
        #   hold on
        #   plot(speed_kts,n_neg,'LineWidth',2);
        lines(speed_kts,
              n_neg,
              col="blue",
              type="l",
              lwd=2)
        
        #plot Vne line
        #   figure(1)
        #   hold on
        #   plot([Vne, Vne],[max(n_pos), min(n_neg)],'LineWidth',2);
        lines(c(Vne,Vne),
              c(max(n_pos), min(n_neg)),
              col="blue",
              type="l",
              lwd=2)
        
        #plot Gust lines
        #   figure(1)
        for(j in 1:length(Vg)){
          #         hold on
          #         plot(speed_kts,n_gust_pos[j,],'m-.')
          lines(speed_kts,
                n_gust_pos[j,],
                col="deeppink",
                type="l",
                lwd=2,
                lty=4)
          #         hold on
          #         plot(speed_kts,n_gust_neg[j,],'m-.')
          lines(speed_kts,
                n_gust_neg[j,],
                col="deeppink",
                type="l",
                lwd=2,
                lty=4)
        }
        
      }
    }
    
    
    #   
    #Clean up output parameters because we had a leading placeholder value
    Vstall_pos = Vstall_pos[-1];
    Va_pos = Va_pos[-1];
    Vstall_neg = Vstall_neg[-1];
    Va_neg = Va_neg[-1];
    
    #   #plot Vs lines
    #   figure(1)
    for(i in 1:length(Vstall_pos)){
      #   hold on
      #   plot([Vstall_pos(i) Vstall_pos(i)],[0 1],'r')
      lines(c(Vstall_pos[i],Vstall_pos[i]),
            c(0,1),
            col="red",
            type="l",
            lwd=1)
      #   hold on
      #   plot([Vstall_neg(i) Vstall_neg(i)],[0 -1],'r')
      lines(c(Vstall_neg[i],Vstall_neg[i]),
            c(0,-1),
            col="red",
            type="l",
            lwd=1)
    }
    #   
    #   #plot Va lines
    #   figure(1)
    for(i in 1:length(Va_pos)){
      if(Va_pos[i]<Vne){
        #   hold on
        #   plot([Va_pos(i) Va_pos(i)],[0 max(DesignLoadFactor)],'b')
        lines(c(Va_pos[i],Va_pos[i]),
              c(0,max(DesignLoadFactor)),
              col="blue",
              type="l",
              lwd=1)
      }
      if(Va_neg[i]<Vne){
        #   hold on
        #   plot([Va_neg(i) Va_neg(i)],[0 min(DesignLoadFactor)],'b')
        lines(c(Va_neg[i],Va_neg[i]),
              c(0,min(DesignLoadFactor)),
              col="blue",
              type="l",
              lwd=1)
      }
    }
  
  })
  
})
