package org.udesa.unoapplication.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

//mapea a pagina web (para hacer apps rest full).
// gestiona entre mi app y puerto que diga
@Controller  //biblioteca que uso
public class UnoController {

    @PostMapping("newmatch") public ResponseEntity newMatch( @RequestParam List<String> players )
    @PostMapping("play/{matchId}/{player}") public ResponseEntity play( @PathVariable UUID matchId, @PathVariable String player, @RequestBody JsonCard card )
    @PostMapping("draw/{matchId}/{player}") public ResponseEntity drawCard( @PathVariable UUID matchId, @RequestParam String player )
    @GetMapping("activecard/{matchId}") public ResponseEntity activeCard( @PathVariable UUID matchId )
    @GetMapping("playerhand/{matchId}") public ResponseEntity playerHand( @PathVariable UUID matchId )
}