package org.udesa.unoapplication.model;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.udesa.unoapplication.service.UnoService;
import org.udesa.unoapplication.service.service;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@SpringBootTest
public class UnoServiceTest {
    @Autowired private service unoService; //dsp poner UnoService class

    @Test public void newMatchTest(){
        UUID id = unoService.newMatch(List.of("Miguel","Jorge"));
        assertNotNull(id);
    }
}
