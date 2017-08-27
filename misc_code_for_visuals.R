library(tidyverse); library(modelr); library(mgcv); library(plotly); library(lazerhawk)

mod = gam(accel ~ s(times, bs='gp'), data=MASS::mcycle)

ax = list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticks = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

MASS::mcycle %>%
  add_predictions(mod) %>%
  plot_ly(x=~times, y=~accel) %>%
  add_markers(color=~accel, colors=viridis::plasma(133), showlegend=F) %>%
  add_lines(y=~pred, color=I('#ff5500'), opacity=.5, showlegend=F) %>%
  hide_colorbar() %>%
  theme_plotly() %>%
  layout(xaxis = ax, yaxis = ax)


input = data_frame(
  n = 10,
  dataMean = 7,
  dataVar = 3,
  priorMean = 2,
  priorVar = 1
)

simN = 500
theta = seq(0, 10, length.out = simN)

obs  = rnorm(input$n, input$dataMean, sqrt(input$dataVar))
prior = data.frame(Distribution='Prior', theta=theta,
                   density = dnorm(theta, input$priorMean, sqrt(input$priorVar))) %>%
  mutate(density=density/sum(density))

like = data.frame(Distribution='Likelihood', theta=theta,
                  density = sapply(theta, function(parm) exp(sum(dnorm(obs, mean=parm, sd=sqrt(input$dataVar), log = T))))) %>%
  mutate(density=density/sum(density))

denom = sum(like$density*prior$density)
post = data.frame(Distribution='Posterior', theta=theta,
                  density = like$density*prior$density/denom) %>%
  mutate(density=density/sum(density))

thetamean = sum(post$density*theta)
plotdata = rbind(prior, like, post)





g = ggplot(aes(x=theta, y=density, group=Distribution, color=Distribution, fill=Distribution), data=plotdata) +
    geom_ribbon(aes(ymin=0, ymax=density), alpha=.5 ) +
    geom_point(aes(x=value, y=0), data=data.frame(Distribution=c('Prior', 'Likelihood', 'Posterior'),
                                                  value=c(input$priorMean, mean(obs), thetamean)),
               color=alpha('#ff5503', .25)) +
    xlab('') +
    lims(x=c(0, 10)) +
    lazerhawk::theme_trueMinimal() +
    theme(axis.title.x=element_text(color=alpha('black',.6), vjust=.1, hjust=.5),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text=element_text(color=alpha('black',.5), vjust=.01),
          legend.position='none',
          plot.background=element_rect(fill = "transparent",colour = NA))

ggplotly(g, tooltip='none') %>%
  theme_plotly() %>%
  layout(xaxis=ax)
