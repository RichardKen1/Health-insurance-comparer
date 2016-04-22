

library(shiny)
library(Rcpp)
library(ggplot2)

cost<-read.csv("age.and.gender/age and gender.csv", header=FALSE)
fillout<- read.csv("fillout.data/Copy of fillout dataset.csv", header=TRUE)
pop<-read.csv("Polished.population/Polished Population Data.csv")
percentile<- read.csv("Polished.percentile/Polished Percentile Data.csv")
p_fillout<- read.csv("probability.fillout/probability_ fillout dataset.csv")


cppFunction('double find_max(DataFrame udf){
            Rcpp::DoubleVector value = udf["Cost"];
            double max=0;
            for(int i=0; i<5; i++){
            if(value[i]>=max)
            max=value[i];
            }
            return max;
            }')

cppFunction('DataFrame find_percentile(DataFrame percentile, DataFrame p_fillout, int age){
            Rcpp::CharacterVector xplot = p_fillout["Medical.Procedure"];
            Rcpp::DoubleVector yplot = p_fillout["Probability"];
            Rcpp:: DoubleVector min_age = percentile["Age.Min"];
            Rcpp:: DoubleVector max_age = percentile["Age.Max"];
            Rcpp:: DoubleVector data_pop = percentile["Medical.Condtions.POP"];
            double true_cost[5];
            double true_per[5];
            
            for(int i=0; i<5; i++){
            true_cost[i]=0;
            true_per[i]=0;
            }
            
            int m=0;
            for(int n=0; n<18; n++)
{
            if(age>=min_age[n] && age<=max_age[n])
            true_per[m]=data_pop[n];
            if(n==3)
            m=1;
            if(n==7)
            m=2;
            if(n==9)
            m=3;
            if(n==13)
            m=4;
}
            
            for(int i=0; i<5; i++){
            yplot[i]=true_per[i];
            }
            
            return DataFrame::create(_["Medical.Procedure"]=xplot,_["Probability"]=yplot );
            }')

cppFunction('DataFrame uninsured_munge(DataFrame fillout, DataFrame cost, DataFrame pop, int age){
            Rcpp::CharacterVector xplot = fillout["Medical.Procedure"];
            Rcpp::DoubleVector yplot = fillout["Cost"];
            Rcpp:: DoubleVector data_cost = cost["V9"];
            Rcpp:: DoubleVector data_pop = pop["Medical.Condtions.POP"];
            Rcpp:: DoubleVector min_age = pop["Age.Min"];
            Rcpp:: DoubleVector max_age = pop["Age.Max"];
            
            double true_cost[5];
            double true_pop[5];
            
            for(int i=0; i<5; i++){
            true_cost[i]=0;
            true_pop[i]=0;
            }
            
            int med_start[5];
            med_start[0]=1028;
            med_start[1]=92;
            med_start[2]=524;
            med_start[3]=416;
            med_start[4]=920;
            
            for(int j=0; j<5; j++)
{
            if(age>=0 && age<=18)
            true_cost[j]=data_cost[med_start[j]];
            else if(age>= 19 && age<= 44){
            med_start[j]=med_start[j]+1;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=45 && age<=64){
            med_start[j]=med_start[j]+2;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=65 && age<=84){
            med_start[j]=med_start[j]+3;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=85 && age<=100){
            med_start[j]=med_start[j]+4;
            true_cost[j]=data_cost[med_start[j]];
            }
            else
            true_cost[j]=0;
}
            
            int m=0;
            for(int n=0; n<18; n++)
{
            if(age>=min_age[n] && age<=max_age[n])
            true_pop[m]=data_pop[n];
            if(n==3)
            m=1;
            if(n==7)
            m=2;
            if(n==9)
            m=3;
            if(n==13)
            m=4;
}
            
            for( int z=0; z<5; z++){
            if(true_pop[z]==0)
            yplot[z]=0;
            else
            yplot[z]=((true_cost[z])/true_pop[z])*1000000;
            }
            
            return DataFrame::create(_["Medical.Procedure"]=xplot,_["Cost"]=yplot );
            }')

cppFunction('DataFrame insured_munge(DataFrame fillout, DataFrame cost, DataFrame pop, int age){
            Rcpp::CharacterVector xplot = fillout["Medical.Procedure"];
            Rcpp::DoubleVector yplot = fillout["Cost"];
            Rcpp:: DoubleVector data_cost = cost["V9"];
            Rcpp:: DoubleVector data_pop = pop["Medical.Condtions.POP"];
            Rcpp:: DoubleVector min_age = pop["Age.Min"];
            Rcpp:: DoubleVector max_age = pop["Age.Max"];
            
            double true_cost[5];
            double true_pop[5];
            
            for(int i=0; i<5; i++){
            true_cost[i]=0;
            true_pop[i]=0;
            }
            
            int med_start[5];
            med_start[0]=992;
            med_start[1]=56;
            med_start[2]=488;
            med_start[3]=380;
            med_start[4]=884;
            
            for(int j=0; j<5; j++)
{
            if(age>=0 && age<=18)
            true_cost[j]=data_cost[med_start[j]];
            else if(age>= 19 && age<= 44){
            med_start[j]=med_start[j]+1;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=45 && age<=64){
            med_start[j]=med_start[j]+2;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=65 && age<=84){
            med_start[j]=med_start[j]+3;
            true_cost[j]=data_cost[med_start[j]];
            }
            else if(age>=85 && age<=100){
            med_start[j]=med_start[j]+4;
            true_cost[j]=data_cost[med_start[j]];
            }
            else
            true_cost[j]=0;
}
            
            int m=0;
            for(int n=0; n<18; n++)
{
            if(age>=min_age[n] && age<=max_age[n])
            true_pop[m]=data_pop[n];
            if(n==3)
            m=1;
            if(n==7)
            m=2;
            if(n==9)
            m=3;
            if(n==13)
            m=4;
}
            
            for( int z=0; z<5; z++){
            if(true_pop[z]==0)
            yplot[z]=0;
            else
            yplot[z]=((true_cost[z])/true_pop[z])*1000000;
            }
            
            return DataFrame::create(_["Medical.Procedure"]=xplot,_["Cost"]=yplot );
            }')


shinyServer(
  function(input, output){
    output$percent_table <-renderTable({find_percentile(percentile, p_fillout, input$age)})
    
    output$uninsured_plot <-renderPlot({
      udf<-uninsured_munge(fillout, cost, pop, input$age)
      max<-find_max(udf)
      ggplot(data=udf, aes(x=Medical.Procedure, y=Cost, fill=Cost)) +
        geom_bar(colour="black", stat="identity") + ylim(0, max+50)+
        ggtitle("Cost Without Insurance")+theme(plot.title=element_text(face="bold"))
    })
    
    output$insured_plot <-renderPlot({
      udf<-uninsured_munge(fillout, cost, pop, input$age)
      idf<-insured_munge(fillout, cost, pop, input$age)
      max<-find_max(udf)
      ggplot(data=idf, aes(x=Medical.Procedure, y=Cost, fill=Cost))+
        geom_bar(colour="black", stat="identity") + ylim(0, max+50)+
        ggtitle("Cost With Insurance")+theme(plot.title=element_text(face="bold"))
    })
  }
)