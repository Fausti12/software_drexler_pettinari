package org.udesa.unoapplication.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.udesa.unoapplication.model.JsonCard;
import org.udesa.unoapplication.service.UnoService;

import java.util.List;
import java.util.UUID;

//mapea a pagina web (para hacer apps rest full).
// gestiona entre mi app y puerto que diga
@Controller  //biblioteca que uso
public class UnoController {
    @Autowired UnoService service;

    @PostMapping("newmatch")
    public ResponseEntity newMatch(@RequestParam List<String> players ){
        return ResponseEntity.ok(service.createNewMatch(players));
    }

    @PostMapping("play/{matchId}/{player}")
    public ResponseEntity play(@PathVariable UUID matchId, @PathVariable String player,
                               @RequestBody JsonCard card ){
        service.play(matchId, player, card.asCard());
        return ResponseEntity.ok().build();
    }

    @PostMapping("draw/{matchId}/{player}")
    public ResponseEntity drawCard( @PathVariable UUID matchId,
                                    @RequestParam String player ){
        service.drawCard(matchId, player);
        return ResponseEntity.ok().build();
    }

    @GetMapping("activecard/{matchId}")
    public ResponseEntity activeCard( @PathVariable UUID matchId ){
        return ResponseEntity.ok(service.visibleCard(matchId).asJson());
    }

    @GetMapping("playerhand/{matchId}")
    public ResponseEntity playerHand( @PathVariable UUID matchId ){
        return ResponseEntity.ok(
                service.handOfCurrentPlayer(matchId).stream().map(c -> c.asJson()).toList()
        );
    }
}