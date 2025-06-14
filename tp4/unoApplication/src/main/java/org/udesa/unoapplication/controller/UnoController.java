package org.udesa.unoapplication.controller;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.udesa.unoapplication.model.JsonCard;
import org.udesa.unoapplication.service.UnoService;


import java.util.List;
import java.util.UUID;

@Controller  //biblioteca que uso
public class UnoController {
    @Autowired
    UnoService unoService; //spring junta instancias que necesito

    // Manejo de errores
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<String> handleIllegal(IllegalArgumentException exc) {
        return ResponseEntity.badRequest().body("Illegal Argument: " + exc.getMessage());
    }

    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<String> handleRuntime(RuntimeException exc) {
        return ResponseEntity.badRequest().body("Error: " + exc.getMessage());
    }

    @ExceptionHandler(HttpMessageNotReadableException.class) //para el Json
    public ResponseEntity<String> handleUnreadable(HttpMessageNotReadableException e) {
        return ResponseEntity.badRequest().body("Malformed or incomplete JSON: " +
                e.getMostSpecificCause().getMessage());
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
        return ResponseEntity.ok().build();
    }

    @GetMapping("activecard/{matchId}")
    public ResponseEntity activeCard( @PathVariable UUID matchId ){
        return ResponseEntity.ok(unoService.activeCard(matchId).asJson());
    }

    @PostMapping("play/{matchId}/{player}")
    public ResponseEntity play(@PathVariable UUID matchId, @PathVariable String player,
                               @RequestBody JsonCard card ){

        unoService.play(matchId, player, card.asCard());
        return ResponseEntity.ok().build();

    }

}