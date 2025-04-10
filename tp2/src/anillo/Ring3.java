package anillo;
/*
import java.util.Stack;

class Nodo { //cambiar nombre a clase
    Object cargo;
    Nodo next;
    Nodo prev;

    Nodo(Object cargo) {
        this.cargo = cargo;
        this.next = this; // Se apunta a sí mismo al principio
        this.prev = this;
    }

    Nodo add(Object newCargo) {
        Nodo newNode = new Nodo(newCargo);
        newNode.next = this;
        this.prev.next = newNode;
        newNode.prev = this.prev;
        this.prev = newNode;
        return newNode;
    }

    Nodo next() { return this.next; }

    Nodo remove(Stack<Nodo> ringStack) {
        //return quienPongo(ringStack);

        Nodo actual = ringStack.pop();       // Sacamos el nodo actual (top del stack)
        Nodo siguiente = actual.next;        // Siguiente del nodo eliminado (nuevo actual)

        // Eliminar nodo actual del anillo
        actual.prev.next = actual.next;
        actual.next.prev = actual.prev;
        //ringStack.remove(1);
        ringStack.push(siguiente);           // El siguiente ahora es el nuevo top
        ringStack.remove(1);
        return siguiente;

    }

    private Nodo quienPongo(Stack<Nodo> ringStack) {
        ringStack.pop();
        Nodo eslabon = ringStack.getLast(); // Devuelve el elemento en la cima de la pila (último elemento insertado) sin eliminarlo.

        eslabon.cargo = this.next.cargo;
        this.next.next.prev = eslabon;
        eslabon.next = this.next.next;

        this.prev.next = eslabon;
        eslabon.prev = this.prev;
        return eslabon;
    }


    Object current() { return cargo; }
}

class NodoVacio extends Nodo {
    NodoVacio() {
        super(null); // se pasa cargo=null a constructor Nodo
        this.next = this; // Se apunta a sí mismo
        this.prev = this;
    }

    @Override //redefinir un metodo de la clase padre en una subclase
    Nodo add(Object cargo) {
        Nodo eslabon = new Nodo(cargo); // Si es vacío, se convierte en un nodo real
        //this.next = eslabon;
        //this.prev = eslabon;
        return eslabon;
    }

    @Override
    Nodo next() {
        throw new RuntimeException("Empty ring");
    }

    @Override
    Nodo remove(Stack<Nodo> ringStack) {
        throw new RuntimeException("Empty ring");
    }

    @Override
    Object current() {
        throw new RuntimeException("No value in ring");
    }
}

public class Ring {
    private Nodo nodo;
    private Stack<Nodo> ringStack = new Stack<>();

    public Ring() {
        nodo = new NodoVacio(); // Siempre empieza con un nodo vacío
        ringStack.push(nodo);
    }

    public Ring add(Object cargo) {
        nodo = nodo.add(cargo);
        ringStack.push(nodo);
        return this;
    }

    public Ring next() {
        //nodo = nodo.next;
        //nodo = nodo.next();
        //return this;

        Nodo anterior = ringStack.pop();        // sacás el nodo actual
        nodo = nodo.next();                     // movés al siguiente
        ringStack.push(nodo);                   // nuevo nodo actual arriba del stack
        ringStack.insertElementAt(anterior, 1);           // el viejo nodo va al fondo del stack
        return this;


    }

    public Object current() { return nodo.current(); }

    public Ring remove() {
        nodo = nodo.remove(ringStack);
        return this;
    }
}
*/
