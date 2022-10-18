ask_user<- function(){
  y <- readline(prompt="what dataset do you want to see: ")
  if(y == ''){
    print("please enter a string value")
  }else if(y == "starwars"){
    return(starwars)
  }else{
    print("no such data")
  }
}

ask_user()

