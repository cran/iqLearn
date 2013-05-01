learnIQ1Est <-
function (mainObj, cmObj, sigObj, dens){
  A1 = cmObj$A1;
  
  if (dens=="norm"){
    normal = T;
  }
  else if (dens=="nonpar"){
    normal = F;
  }
  else{
    stop ("dens must be one of {norm, nonpar}");
  }
  ## NEXT: estimate optimal treatment for each patient in training
  ## set

  ## contrast function mean estimate
  muPos = cmObj$cmPos;
  muNeg = cmObj$cmNeg;

  ## lhat is the estimate of the expectation of the main effect term
  lhatPos = mainObj$mainPos; 
  lhatNeg = mainObj$mainNeg;

  ## standard deviation estimates
  sigPos = sigObj$sigPos;
  sigNeg = sigObj$sigNeg;

  ## Estimate Q1 for both A1=1 and A1=-1 using either normal density
  ## or empirical estimate
  if (normal){
    q1HatPos = lhatPos + muPos*(1-2*pnorm (-muPos/sigPos)) +
      sqrt(2/pi)*sigPos*exp (-muPos^2/(2*sigPos^2)); 
    q1HatNeg = lhatNeg + muNeg*(1-2*pnorm (-muNeg/sigNeg)) +
      sqrt(2/pi)*sigNeg*exp (-muNeg^2/(2*sigNeg^2)); 
  }
  else{
    opPos = sum (abs (muPos + sigPos*sigObj$stdResids)*(A1==1))/sum (A1==1);
    opNeg = sum (abs (muNeg + sigNeg*sigObj$stdResids)*(A1==-1)) /
      sum(A1==-1); 
    q1HatPos = lhatPos + opPos;
    q1HatNeg = lhatNeg + opNeg;
  }

  ## Vector of optimal first-stage txts
  optA1 = sign (q1HatPos - q1HatNeg);

  list ("optA1"=optA1)
}
