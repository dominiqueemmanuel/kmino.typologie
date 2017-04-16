init_profil_var_msg <- function(){
  profil_var_msg <- data.frame(profil_var_msg_id=""
                     ,profil_var_msg_fr=""
                     ,stringsAsFactors = FALSE)

  profil_var_msg[1,]$profil_var_msg_id <- "profil_var_msg_000001"
  profil_var_msg[2,]$profil_var_msg_id <- "profil_var_msg_000002"
  profil_var_msg[3,]$profil_var_msg_id <- "profil_var_msg_000003"
  profil_var_msg[4,]$profil_var_msg_id <- "profil_var_msg_000004"
  profil_var_msg[5,]$profil_var_msg_id <- "profil_var_msg_000005"
  profil_var_msg[6,]$profil_var_msg_id <- "profil_var_msg_000006"
  profil_var_msg[7,]$profil_var_msg_id <- "profil_var_msg_000007"
  profil_var_msg[8,]$profil_var_msg_id <- "profil_var_msg_000008"
  profil_var_msg[9,]$profil_var_msg_id <- "profil_var_msg_000009"
  profil_var_msg[10,]$profil_var_msg_id<- "profil_var_msg_000010"
  profil_var_msg[11,]$profil_var_msg_id<- "profil_var_msg_000011"
  profil_var_msg[12,]$profil_var_msg_id<- "profil_var_msg_000012"
  profil_var_msg[13,]$profil_var_msg_id<- "profil_var_msg_000013"
  profil_var_msg[14,]$profil_var_msg_id<- "profil_var_msg_000014"

  profil_var_msg[1,]$profil_var_msg_fr <- "Variable quantitative"
  profil_var_msg[2,]$profil_var_msg_fr <- "Variable qualitative"
  profil_var_msg[3,]$profil_var_msg_fr <- "Variable qualitative ordonnée"
  profil_var_msg[4,]$profil_var_msg_fr <- "Variable date"
  profil_var_msg[5,]$profil_var_msg_fr <- "Variable caractère"
  profil_var_msg[6,]$profil_var_msg_fr <- "Variable présentant des valeurs manquantes"
  profil_var_msg[7,]$profil_var_msg_fr <- "Variable sans valeurs manquantes"
  profil_var_msg[8,]$profil_var_msg_fr <- "Variable dont les modalités sont à effectifs égaux"
  profil_var_msg[9,]$profil_var_msg_fr <- "Variable constante"
  profil_var_msg[10,]$profil_var_msg_fr<- "Variable présentant des valeurs extrêmes"
  profil_var_msg[11,]$profil_var_msg_fr<- "Variable présentant des points d'accumulation"
  profil_var_msg[12,]$profil_var_msg_fr<- "Variable présentant des doublons"
  profil_var_msg[13,]$profil_var_msg_fr<- "Variable présentant des valeurs nulles"
  profil_var_msg[14,]$profil_var_msg_fr<- "profil_var_msg_000014"

  # Patch sur bug (?) rownames
  rownames(profil_var_msg) <- seq(1:dim(profil_var_msg)[1])

  return(profil_var_msg)
}
init_profil_var_msg()
# profil_var_msg

# Fonction ajout texte f(langue, msg)
# Fonction ajout trad f(id,msg)
# Fonction edit text f(id,langue,msg)
# Fonction suppress text f(id)
# Fonction renvoi text f(numid)
