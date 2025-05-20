package uno;


import java.util.ArrayList;
import java.util.List;


public class Jugador {
    // borrado el atributo nombre de jugador
    private final List<Carta> mano = new ArrayList<>();
    //private boolean cantoUno = false;

    public void recibir(Carta carta) {
        mano.add(carta);
    }

    public void jugar(Carta carta) {
        //System.out.println(nombre);
        if (!mano.contains(carta)) throw new IllegalArgumentException("La carta no está en la mano");
        mano.remove(carta);
        //cantoUno = carta.fueCantadoUno();
    }

    //public boolean tieneUnaCarta() {return mano.size() == 1;}

    //public boolean cantoUno() {return cantoUno;}

    public int cantidad() {
        return mano.size();
    }
}
