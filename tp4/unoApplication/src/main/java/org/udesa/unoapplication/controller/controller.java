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

    // Manejo de errores directamente acá
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<String> handleIllegal(IllegalArgumentException exc) {
        System.err.println("IllegalArgumentException: " + exc.getMessage()); // o usar logger
        return ResponseEntity.badRequest().body("Error inesperado. Intente nuevamente.");
    }

    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<String> handleRuntime(RuntimeException exc) {
        System.err.println("RuntimeException: " + exc.getMessage()); // o usar logge
        return ResponseEntity.badRequest().body("Error: " + exc.getMessage());
    }

    @PostMapping ("newmatch")
    public ResponseEntity newMatch(@RequestParam List<String> players){
        return ResponseEntity.ok(unoService.newMatch(players));
    }

    @GetMapping("playerhand/{matchId}")
    public ResponseEntity playerHand( @PathVariable UUID matchId ){
        return ResponseEntity.ok(
                unoService.playerHand(matchId).stream().map(each -> each.asJson())
        );
    }

    @PostMapping("draw/{matchId}/{player}")
    public ResponseEntity drawCard( @PathVariable UUID matchId, @PathVariable String player ){
        unoService.drawCard(matchId, player);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @GetMapping("activecard/{matchId}")
    public ResponseEntity activeCard( @PathVariable UUID matchId ){
        return ResponseEntity.ok(unoService.activeCard(matchId).asJson());
    }

    @PostMapping("play/{matchId}/{player}")
    public ResponseEntity play(@PathVariable UUID matchId, @PathVariable String player,
                               @RequestBody JsonCard card ){
        unoService.play(matchId, player, card.asCard());
        return new ResponseEntity<>(HttpStatus.OK);
    }




}