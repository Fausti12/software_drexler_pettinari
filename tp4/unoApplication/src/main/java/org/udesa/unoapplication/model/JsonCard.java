package org.udesa.unoapplication.model;

import lombok.Getter;
import lombok.SneakyThrows;

import java.util.List;


@Getter
public class JsonCard {
    String color;
    Integer number;
    String type;
    boolean shout;

    public JsonCard() {}
    public JsonCard( String color, Integer number, String type, boolean shout ) {
        //se agrega chequeo de color y número
        if (!List.of("Red", "Green", "Blue", "Yellow").contains(color)) {
            throw new IllegalArgumentException("Invalid color: " + color);
        }

        if (!List.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).contains(number)) {
            throw new IllegalArgumentException("Invalid number: " + number);
        }

        this.color = color;
        this.number = number;
        this.type = type;
        this.shout = shout;
    }

    public String toString() {
        return "{ " +
               "\"color\": \"" + color + "\", " +
               "\"number\": \"" + number + "\", " +
               "\"type\": \"" + type + "\", " +
               "\"shout\": \"" + shout + "\" " +
               "}";
    }

    @SneakyThrows public Card asCard() {
        return (Card)Class.forName( "org.udesa.unoapplication.model." + type )
                .getMethod( "asCard", getClass() )
                .invoke( getClass(), this );
    }
}

