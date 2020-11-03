# Shiny APP para implementar interactivamente la dinamica de los elementos bayesianos en funcion de la ncertidumbre
library(shiny);library(ggplot2);library(shinyMobile)

#define colors and slider width in px
dens_cols=c("orange","pink","lightblue", "green") # prior, like, post, evidence
slid_wd="100%"
slid_wd2="75%"

hist_ui = f7Page(
  title = "Bayes para todes",
  dark_mode = T,
  init = f7Init(skin = "auto", theme = "dark",color = "lightblue"),
  f7SingleLayout(
    panels =  
      f7Panel(effect = "cover",resizable = T,
        tagList(  
          f7Slider(
            inputId = "mpS",
            label = h5("¿Qué sabés de antemano?",tags$br(),"[Media Prior]:"),
            color = dens_cols[1],
            min = -50,
            max = 50,
            labels = tagList(f7Icon("minus_circle"),
                             f7Icon("plus_circle")),
            value = -10,
            step = .5
          ),
          f7Slider(
            inputId = "spS",
            label = h5("¿Cuán incierta es esa creencia previa?",tags$br(),"[Desvío Prior]:"),
            color = dens_cols[1],
            min = 0.1,
            max = 15,
            labels = tagList(f7Icon("minus_circle"),
                             f7Icon("plus_circle")),
            value = 2.5,
            step = .3
          ),
          
          f7Slider(
            inputId = "spXS",
            label = h5("¿Cuán ruidoso es el sistema?",tags$br(),"[Desvío Likelihood]:"),
            min = 0.1,
            max = 15,
            labels = tagList(f7Icon("minus_circle"),
                             f7Icon("plus_circle")),
            color=dens_cols[2],
            value = 2,
            step = .3 
          ),
          
          f7Slider(
            inputId = "xi",
            label = h5("¿Qué información nueva recibiste?",tags$br(),"[Evidencia]:"),
            min = -50,
            color=dens_cols[4],
            labels = tagList(f7Icon("minus_circle"),
                             f7Icon("plus_circle")),
            max = 50,
            value = 15,
            step = .1 
          ),
        ),
        
        side = "left",
        title = "Controles"),
    
    navbar = 
      f7Navbar(
        
        title = 
          tagList(
            f7Icon("arrowtriangle_left_fill",color = "lightblue",style = "font-size:20px"),
            ("Toca aca.")
            
          ), 
        subNavbar = NULL, 
        subtitle = NULL,
        hairline = TRUE, 
        shadow = TRUE, 
        left_panel = TRUE, 
        right_panel = FALSE
      ),
    f7Popup(
      title = tagList(
        h2("De tendencias e incertezas.", align = "center"), 
        h2("¿Cómo construimos nuestras creencias?", align = "center",
           style="white-space:normal;word-wrap: break-word;"),
        p("La idea de esta app es generar intuiciones acerca de los conceptos clave del razonamiento Bayesiano.",
          style="white-space:normal;word-wrap: break-word;"),br(),
        p("Usando el menu de la izquierda, vas a poder variar todos los parametros que reflejan propiedades tanto del observador como 
          del mundo que lo rodea.",
          style="white-space:normal;word-wrap: break-word;"),br(),
        p("Despues de jugar, espero que esta formula (la regla de Bayes) tenga mucho mas sentido: ",
          style="white-space:normal;word-wrap: break-word;"),
        img(src = "br.jpg", height = 200, width = 240,style="display: block; margin-left: auto; margin-right: auto;"),
        p("Esta app es super silenciosa, asi que te recomiendo apretar el signo de pregunta con +info para complementar.",
          style="white-space:normal;word-wrap: break-word;")
      ),
      backdrop = TRUE,
      closeByBackdropClick = TRUE,
      closeOnEscape = T,
      animate = TRUE,
      swipeToClose = FALSE,
      fullsize = FALSE,
      closeButton = TRUE,
      id = "popup1"
    ),
    # plot
    f7Shadow(
      intensity = 20,
      hover = TRUE,

      f7Card(
        plotOutput("distPlot",width = "95%"),
        f7Slider(
          inputId = "range",
          label = h3("Rango a graficar:"),
          color="white",
          min = -70,
          max = 70,
          value = c(-25,25),
          labels = tagList(f7Icon("backward_fill"),
                           f7Icon("forward_fill")),
          step = 1
        ),
        br(),
        span(style = paste0("color:",dens_cols[1],";font-weight:bold;font-size: 110%;"), textOutput("prior") ) ,
        br(),
        span(style = paste0("color:",dens_cols[2],";font-weight:bold;font-size: 110%;"), textOutput("like") )  ,
        br(),
        span(style = paste0("color:",dens_cols[3],";font-weight:bold;font-size: 110%;"), textOutput("posterior") ),
        footer =
          tagList(
            f7Link(
              external = T,
              icon = f7Icon("question_circle_fill"),
              label = "+ info",
              src = "https://es.wikipedia.org/wiki/Teorema_de_Bayes"
            ),
            h4("@2exp3 - Agustin PS")
          )
      )
    )
  )
)

hist_server = function( input, output ) {
  observeEvent(input$shinyInfo, {
    f7TogglePopup(id = "popup1")
  })
  
  rxn=reactive({    
    paso=0.1
    
    mean.pS=input$mpS
    sd.pS=input$spS 
    xi=input$xi
    si=seq(input$range[1],input$range[2],paso) # esto capaz  mejor dejarlo fijo
    sd.pXS=input$spXS  #seq(1.3,7,by=.08)
    
    # prior
    pS=dnorm(x = si, mean = mean.pS,sd = sd.pS)
    #like
    pXS= dnorm(x=xi, mean=si, sd=sd.pXS)
    #protopost
    pp=pXS*pS
    #posterior
    pSX=pp/(sum(pp))
    # densidad
    dSX=pSX/paso
    dSXMAP=si[which.max(dSX)]
    dSXsigma=round((dnorm(x=dSXMAP, mean = dSXMAP, sd = 1) )/max(dSX),3) 
    # todo
    distris=data.frame(s=si, prior=pS, likelihood=pXS, posterior=dSX)
    params=data.frame(mpri=mean.pS,sdpri=sd.pS,
                      xi=xi, sdlik=sd.pXS,
                      MAP=dSXMAP, SD=dSXsigma)
    valores=c(dSXMAP,dSXsigma)
    devu=list(distris,params,valores)
    return(devu)} 
  )
  
  # output (plot) ====
  output$distPlot <- renderPlot( 
    {
      distris=rxn()[[1]]
      params=rxn()[[2]]
      # PLOT
      ggplot(data=distris, aes(x=s))+
        # prior
        geom_line(aes(y=prior), color=dens_cols[1], lwd=.9)+
        geom_area(aes(y=prior),fill=dens_cols[1], alpha=.3)+
        geom_segment(aes(x=params$mpri, xend=params$mpri, 
                         y=0, yend=max(prior) ), 
                     lty=2, color=dens_cols[1], lwd=1)+
        
        #like
        geom_line(aes(y=likelihood), color=dens_cols[2], lwd=.9)+
        geom_area(aes(y=likelihood),fill=dens_cols[2], alpha=.3)+
        geom_segment(aes(x=params$xi, xend=params$xi, 
                         y=0, yend=max(likelihood) ),
                     lty=2, color=dens_cols[2], lwd=1)+
        
        #posterior
        geom_line(aes(y=posterior), color=dens_cols[3], lwd=1.25)+
        geom_area(aes(y=posterior),fill=dens_cols[3], alpha=.8)+
        geom_segment(aes(x=params$MAP, xend=params$MAP, 
                         y=0, yend=max(posterior) ), lty=2, color=dens_cols[3], lwd=1.2)+
        
        # evidence 
        geom_point(x=params$xi, y=0, size=8,color="black")+
        geom_point(x=params$xi, y=0, size=5,color="white",alpha=.7)+
        geom_point(x=params$xi, y=0, size=5,color=dens_cols[4])+
        
        labs(x="\nHipótesis/Estímulo\n", y="Densidad de Probabilidad\n")+
        theme_void()+
        theme(
          text=element_text(size=18,color = "lightgray"),
          axis.text = element_text(colour = "lightgray"),
          axis.title.y = element_text(angle = 90),
          axis.title.x = element_text(siz=18),
          plot.background = element_rect(fill = "#15191C"),
          panel.background = element_rect(fill = "transparent", color=NA),
          panel.border = element_blank(),
          panel.grid = element_blank()
        )
    } 
  )
  
  output$prior = renderText(paste("La media de tu PRIOR es:",input$mpS,", y el desvío:", input$spS))
  output$like = renderText(paste("La media de tu LIKELIHOOD es:",input$xi,", y el desvío:", input$spXS))
  output$posterior = renderText(paste("El máximo de tu POSTERIOR (MAP) es:",
                                      round(rxn()[[3]][1],2),
                                      ", y el desvío:",
                                      round(rxn()[[3]][2],2) ))
  
}

shinyApp(ui = hist_ui,
         server = hist_server)

