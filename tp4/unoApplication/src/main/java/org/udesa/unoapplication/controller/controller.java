package org.udesa.unoapplication.controller;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.*;
import org.udesa.unoapplication.model.JsonCard;
import org.udesa.unoapplication.service.service;

import java.util.List;
import java.util.UUID;

@Controller  //biblioteca que uso
public class controller {
    @Autowired service unoService; //spring junta instancias que necesito

    @GetMapping("/hola") //get=levantar algo
    public ResponseEntity<String> holaMundo(){
        return new ResponseEntity<>("respuesta a Hola Mundo", HttpStatus.OK);
    }

    @PostMapping ("newmatch")
    public ResponseEntity newMatch(@RequestParam List<String> players){
        //return ResponseEntity.ok(UUID.randomUUID()); //te crea uno y te manda lo que está dentrod de ok()
        return ResponseEntity.ok(unoService.newMatch(players));
    }

    //copiar el playerHand de jorge

    //




    }