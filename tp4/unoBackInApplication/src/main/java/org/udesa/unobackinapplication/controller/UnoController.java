package org.udesa.unobackinapplication.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

//mapea a pagina web (para hacer apps rest full). No es necesario para el tp por lo que dijo jorge
// gestiona entre mi app y puerto que diga
@Controller  //biblioteca que uso
public class UnoController {
    //notación que indica que mapeamos algo a pagina web:
    @GetMapping("/")  //voy a mapaear a la raiz de pag web este msj
        public String saludo(){
        return "estos son los alumnos de emilio";
    }
    //ahora necesito archivo html (se guardan en resources) -> agrego index.html en templates
}
