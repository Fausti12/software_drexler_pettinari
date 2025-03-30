package anillo;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Ring {
    private Object cargo;
    private Ring nextRing;

    public Ring() {
        nextRing = null;
        cargo = null;
    }

    public Ring next() {
        if ( nextRing == null ) { throw new RuntimeException( "Empty ring" );}
        return nextRing; // Siempre retorna el siguiente nodo, aunque sea el mismo
    }

    public Object current() {
        if ( cargo == null ) { throw new RuntimeException( "No value in ring" );}
        return cargo;}

    public Ring add( Object cargo ) {
        Ring newRing = new Ring();
        //newRing.cargo = cargo;

        if (this.nextRing == null) { // Si el anillo está vacío
            newRing.nextRing = newRing; // Se apunta a sí mismo
            newRing.cargo = cargo;
            return newRing;
        }

        //this.nextRing = newRing; //ver pq es así
        //return newRing;

        //de esta forma se inserta antes del actual
        newRing.cargo = this.cargo; // Copia el contenido actual en el nuevo nodo
        newRing.nextRing = this.nextRing; // Conecta al siguiente nodo

        this.cargo = cargo;
        this.nextRing = newRing;

        return this;
    }

    public Ring remove() {
        if ( nextRing == null ) {
            throw new RuntimeException( "Empty ring" );
        }
        if (nextRing == this) { // Solo hay un nodo en el anillo
            this.cargo = null;
            this.nextRing = null;
            return this;
        }

        // Ring newRing = nextRing;
        //nextRing = nextRing.nextRing;
        //return newRing;
        this.cargo = this.nextRing.cargo;
        this.nextRing = this.nextRing.nextRing;
        //de esta forma el puntero sigue en el mismo nodo pero con el contenido actualizado.
        return this;
    }
}
