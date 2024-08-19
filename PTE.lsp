; // SECTION - Variaveis globais

(setq tamanho-do-texto 3) ; // Altere o tamanho do texto por aqui


(setq espaco-vertical-entre-textos (* tamanho-do-texto 3.33))
(setq espaco-horizontal-entre-textos (* tamanho-do-texto 10))

(setq tamanho-da-linha-horizontal (* espaco-horizontal-entre-textos 4))

(defun horizontal-para-vertical (textos ponto-x ponto-y / novo-ponto-y texto_atual contador_auxiliar)
  
  (setq novo-ponto-y ponto-y)
  
  (repeat (setq contador_auxiliar (sslength textos))
    
    (setq texto_atual (vla-get-textstring (vlax-ename->vla-object (ssname textos (setq contador_auxiliar (1- contador_auxiliar))))))
    
    (vla-put-alignment
      (vla-addtext model_space texto_atual (vlax-3d-point ponto-x novo-ponto-y 0) tamanho-do-texto)
      acAlignmentMiddleCenter
    )
    
    (setq novo-ponto-y (- novo-ponto-y espaco-vertical-entre-textos))
    
  ) ; repeat 
  
  (princ) ; // RETURNS Nothing

) ; defun


(defun desenha-header (ponto-x ponto-y / coordenada-x coordenada-y coordenada-x-final coordenada-y-final meio-header-x meio-header-y)
  
  ; // SECTION - Desenha Linha Horizontal
  (setq coordenada-x (- ponto-x (/ espaco-horizontal-entre-textos 2)))
  
  (setq coordenada-y (+ ponto-y espaco-vertical-entre-textos))
  (setq coordenada-y (+ coordenada-y (/ espaco-vertical-entre-textos 2)))
  (setq coordenada-x-final (+ coordenada-x tamanho-da-linha-horizontal))
  
  (vla-put-layer
    (vla-addline model_space (vlax-3d-point coordenada-x coordenada-y 0) (vlax-3d-point coordenada-x-final coordenada-y 0))
    "0"
  )
  
   ; // SECTION - Desenha Linhas verticais
  (setq coordenada-y-final (- coordenada-y espaco-vertical-entre-textos))
  
  (vla-put-layer
    (vla-addline model_space (vlax-3d-point coordenada-x coordenada-y 0) (vlax-3d-point coordenada-x coordenada-y-final 0))
    "0"
  )
  
  (vla-put-layer
    (vla-addline model_space (vlax-3d-point coordenada-x-final coordenada-y 0) (vlax-3d-point coordenada-x-final coordenada-y-final 0))
    "0"
  )
  
  ; // SECTION - Escreve TABELA no centro
  (setq meio-header-x (/ (+ coordenada-x coordenada-x-final) 2))
  (setq meio-header-y (/ (+ coordenada-y coordenada-y-final) 2))
  
  (vla-put-alignment
    (vla-addtext model_space "TABELA" (vlax-3d-point meio-header-x meio-header-y 0) tamanho-do-texto) 
    acAlignmentMiddleCenter
  )

) ; defun


(defun desenha-tabela (quantidade-de-estacas ponto-x ponto-y / tamanho-da-linha-vertical coordenada-x coordenada-y coordenada-x-final coordenada-y-final)
  
  (setq coordenada-x (- ponto-x (/ espaco-horizontal-entre-textos 2)))
  (setq coordenada-y (+ ponto-y (/ espaco-vertical-entre-textos 2)))
  
  (setq coordenada-x-final (+ coordenada-x tamanho-da-linha-horizontal))

  (repeat (+ quantidade-de-estacas 1)

    (vla-put-layer
      (vla-addline model_space (vlax-3d-point coordenada-x coordenada-y 0) (vlax-3d-point coordenada-x-final coordenada-y 0))
      "0"
    )
    
    (setq coordenada-y (- coordenada-y espaco-vertical-entre-textos))
    
  ); repeat
 
  ; // SECTION - Desenha as linha verticais
  
  (setq coordenada-x (- ponto-x (/ espaco-horizontal-entre-textos 2)))
  (setq coordenada-y (+ ponto-y (/ espaco-vertical-entre-textos 2)))
  
  (setq tamanho-da-linha-vertical (* quantidade-de-estacas espaco-vertical-entre-textos ))
  
  (setq coordenada-y-final (- coordenada-y tamanho-da-linha-vertical))
  
  (repeat 5
      
    (vla-put-layer 
      (vla-addline model_space (vlax-3d-point coordenada-x coordenada-y 0) (vlax-3d-point coordenada-x coordenada-y-final 0))
      "0"
    )
    
    (setq coordenada-x (+ coordenada-x espaco-horizontal-entre-textos))
    
  ) ; repeat 
  
  (PRINC) ; Returns nothing
  
); defun


(defun c:PTE (/ estacas cota-do-terreno cota-geratriz-inferior extensoes ponto-da-tabela acadObj doc model_space previous_osmode ponto-x ponto-y)
  
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg) "*BREAK, *CANCEL*, *EXIT*") 
      (alert (strcat "ERROR: " msg "**"))
    )
    
    (setvar 'osmode previous_osmode)
  )
  
  (setq 
    acadObj (vlax-get-acad-object)
    doc (vla-get-activedocument acadObj)
    model_space (vla-get-modelspace doc)
  )

  (setq previous_osmode (getvar 'osmode))
  (setvar 'osmode 0)
  
  (princ "\nSelecione as estacas apenas:")
  (setq estacas (ssget '(0 . "TEXT")))
  
  (princ "\nSelecione as cota do Terreno:")
  (setq cota-do-terreno (ssget '(0 . "TEXT")))
   
  (princ "\nSelecione as cota da geratriz inferior:")
  (setq cota-geratriz-inferior (ssget '(0 . "TEXT")))
  
  (princ "\nSelecione as extensoes:")
  (setq extensoes (ssget '(0 . "TEXT")))

  
  (setq ponto-da-tabela (getpoint "\nSelecione onde quer que a tabela seja gerada"))
  
  (setq ponto-x (car ponto-da-tabela))
  (setq ponto-y (cadr ponto-da-tabela))
  
  
  (horizontal-para-vertical estacas ponto-x ponto-y)
  (setq ponto-x (+ ponto-x espaco-horizontal-entre-textos))
  
  
  (horizontal-para-vertical cota-do-terreno ponto-x ponto-y)
  (setq ponto-x (+ ponto-x espaco-horizontal-entre-textos))
  
  
  (horizontal-para-vertical cota-geratriz-inferior ponto-x ponto-y)
  (setq ponto-x (+ ponto-x espaco-horizontal-entre-textos))
  
  
  (horizontal-para-vertical extensoes ponto-x ponto-y)
  
  ; // NOTE - Reseta valores
  (setq ponto-x (car ponto-da-tabela))
  (setq ponto-y (cadr ponto-da-tabela))
  
  (desenha-header ponto-x ponto-y)
  (desenha-tabela (sslength estacas) ponto-x ponto-y)
  
  (setvar 'osmode previous_osmode)
  
  (princ) ; // Returns nothing 
  
) ; defun

(alert "Lisp carregada com sucesso! Digite \"PTE\" para comecar.")