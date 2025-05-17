package anillo;

abstract class Link {
    public abstract Link add(Object cargo);
    public abstract Link next();
    public abstract void remove();
    public abstract Object current();

    public abstract Link getNext();
    public abstract Link getPrev();
    public abstract void setNext(Link link);
    public abstract void setPrev(Link link);
}

class DataLink extends Link {
    private Object cargo;
    private Link next;
    private Link prev;

    DataLink(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public Link add(Object cargo) {
        DataLink newNode = new DataLink(cargo);
        newNode.setNext(this);
        newNode.setPrev(this.getPrev());
        this.getPrev().setNext(newNode);
        this.setPrev(newNode);
        return newNode;
    }

    public Link next() {
        return next;
    }

    public void remove(){
        this.cargo = this.next.current();
        this.next = this.next.getNext();
        this.prev = this.prev.getPrev();
    }

    public Object current() {
        return cargo;
    }

    public Link getNext() {
        return next;
    }

    public Link getPrev() {
        return prev;
    }

    public void setNext(Link next) {
        this.next = next;
    }

    public void setPrev(Link prev) {
        this.prev = prev;
    }

}


class EmptyLink extends Link {

    public Link add(Object cargo) {
        return new DataLink(cargo);
    }

    public Link next() {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public void remove() {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current() {
        throw new RuntimeException("current() no permitido en anillo vacío");
    }

    public Link getPrev() {return  null;};

    public Link getNext() {return null;}

    public void setNext(Link next) {};
    public void setPrev(Link prev) {}


}
