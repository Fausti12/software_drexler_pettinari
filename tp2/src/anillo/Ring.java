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
        newRing.cargo = cargo;

        if (this.nextRing == null) { // Si el anillo está vacío
            newRing.nextRing = newRing; // Se apunta a sí mismo
        }
        else {
            newRing.nextRing = this.nextRing; // Inserta en la lista
        }

        this.nextRing = newRing; //ver pq es así
        return newRing;
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

        Ring newRing = nextRing;
        nextRing = nextRing.nextRing;
        return newRing;
    }
}
