#include <TMB.hpp> 
#include <iostream>

template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

template<class Type>
  Type objective_function<Type>::operator() ()
{

    //input data; 
    DATA_VECTOR(x);  
    DATA_VECTOR(se); 
    DATA_IVECTOR(year);  
    DATA_IVECTOR(age);     
  
    int n = x.size(); 

    // parameters;  
    PARAMETER_VECTOR(age_eff);  
    PARAMETER_VECTOR(year_eff);
    PARAMETER_VECTOR(cohort_eff); 
    PARAMETER_VECTOR(log_std);  
    PARAMETER_VECTOR(logit_rho);  
    PARAMETER_ARRAY(dev);  
        
    // transformations
    vector<Type> rho = invlogit(logit_rho);
    vector<Type> std = exp(log_std);
        
    Type rho_year_eff = rho(0); 
    Type rho_cohort_eff = rho(1);
    Type rho_dev_age = rho(2);  
    Type rho_dev_year = rho(3);
     
    Type std_year_eff = std(0);
    Type std_cohort_eff = std(1); 
    Type std_dev = std(2);
  
    // predictions
    vector<Type> pred(n); 
    vector<Type> resid(n); 
    vector<Type> std_resid(n);       
    int a, y, c; 

    for(int i = 0;i < n;++i){
      a = age(i) - age.minCoeff();
      y = year(i) - year.minCoeff();
      c = y + age.maxCoeff() - age(i);
      pred(i) = age_eff(a) + year_eff(y) + cohort_eff(c) + dev(y,a); 
    }
    
    //// likelihood (see https://kaskr.github.io/adcomp/_book/Densities.html)
    Type nll = 0;   
    using namespace density;

    for(int i=0;i<n;i++){
     if(!isNA(x(i))){
      resid(i) = x(i) - pred(i);
      std_resid(i) = resid(i)/se(i);  
      nll -= dnorm(resid(i),Type(0),se(i),true); //Index OBSERVATION MODEL
     }    
    }   
 
    nll += SCALE(AR1(rho_year_eff),std_year_eff)(year_eff);    //year effects
    nll += SCALE(AR1(rho_cohort_eff),std_cohort_eff)(cohort_eff);    //cohort effects
    nll += SCALE(SEPARABLE(AR1(rho_dev_age),AR1(rho_dev_year)),std_dev)(dev);    // dev effects 
        
    // output
  	REPORT(age_eff); 
  	REPORT(year_eff); 
  	REPORT(cohort_eff); 
  	REPORT(dev);        
  	REPORT(resid);     
  	REPORT(std_resid);    
  	REPORT(rho_year_eff);  
  	REPORT(rho_cohort_eff);  
  	REPORT(rho_dev_age);   
  	REPORT(rho_dev_year);    
  	REPORT(std_year_eff);  
  	REPORT(std_cohort_eff);  
  	REPORT(std_dev);                     
    REPORT(pred); 

    ADREPORT(pred);
      
  return nll; 
  } 
