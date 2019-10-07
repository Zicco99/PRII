package com.company;

import java.util.Scanner;
import java.util.Vector;

class veicolo {
    protected String modello;
    protected String targa;
    protected String proprietario;

    // Costruttore
    public veicolo(String mod, String tar, String prop) {
        modello = mod;
        targa = tar;
        proprietario = prop;
    }
    static class furgone extends veicolo{
        protected int capacità;
        public furgone(String mod, String tar, String prop,int cap) {
            super(mod, tar, prop);
            capacità=cap;
        }
    }

    static class moto extends veicolo{
        protected String tipo;
        public moto(String mod, String tar, String prop,String tip) {
            super(mod, tar, prop);
            tipo=tip;
        }
    }
    static class auto extends veicolo{
        protected int posti;
        public auto(String mod, String tar, String prop,int pos) {
            super(mod, tar, prop);
            posti=pos;
        }
    }

}

class Garage{
    private Vector<veicolo> slots;
    int rimanenti;
    //Costruttore
    public Garage(int dim){
        slots=new Vector<veicolo>(dim);
        rimanenti=dim;
    }
    public void add(veicolo sos){                                                         // AGGIUNGI //
        if(rimanenti<0) {
            System.out.println("*Spazio Terminato*");
            return;
        }
        if(sos instanceof veicolo.furgone) {
            System.out.println("*Furgone Aggiunto*\n");
            slots.add(sos);
            rimanenti--;
        }
        if(sos instanceof veicolo.auto) {
            System.out.println("*Auto Aggiunta*\n");
            slots.add(sos);
            rimanenti--;
        }
        if(sos instanceof veicolo.moto) {
            System.out.println("*Moto Aggiunta*\n");
            slots.add(sos);
            rimanenti--;
        }
        System.out.println("*Spazio Rimanente*"+rimanenti +"\n");
    }

    public void stato() {                                                          //CONTROLLA//
        System.out.println("Veicoli presenti:\n");
        for (int i = 0; i < slots.size(); i++) {
            if (slots.get(i) instanceof veicolo.furgone) {
                System.out.println("Furgone: " + " " + slots.get(i).modello + " - " + slots.get(i).targa + " Proprietario: " + slots.get(i).proprietario);
            } else if (slots.get(i) instanceof veicolo.moto) {
                System.out.println("Moto: " + " " + slots.get(i).modello + " - " + slots.get(i).targa + " Proprietario: " + slots.get(i).proprietario);
            } else {
                System.out.println("Auto: " + " " + slots.get(i).modello + " - " + slots.get(i).targa + " Proprietario: " + slots.get(i).proprietario);
            }
        }
        System.out.println("*Spazio Rimanente*"+rimanenti +"\n");
    }


    public void ricercatarga (String pl){
        for (int i = 0; i < slots.size(); i++) {

            if (slots.get(i).targa.contains(pl)) {
                System.out.println("Auto: " + " " + slots.get(i).modello + " - " + slots.get(i).targa + " Proprietario: " + slots.get(i).proprietario);
            }
        }
    }
    public void ricercaprop (String pl){
        for (int i = 0; i < slots.size(); i++) {

            if (slots.get(i).proprietario.contains(pl)) {
                System.out.println("Auto: " + " " + slots.get(i).modello + " - " + slots.get(i).targa + " Proprietario: " + slots.get(i).proprietario);
            }
        }
    }

    public void quitveicolo (String target){
        for (int i=0;i<slots.size();i++){
            if (slots.get(i).targa.equals(target)){
                slots.remove(i);
            }
        }
    }
}

public class Main{
    public static void main(String[] args){
        int exit=0;
        System.out.println("***Gestore Garage***");
        Scanner input=new Scanner(System.in);
        System.out.println("Dammi il numero di slots");
        int garage_dim = Integer.parseInt(input.nextLine());
        Garage garage=new Garage(garage_dim);
        int func;
        while (exit==0) {
            System.out.println("Cosa vuoi fare:\n(1) Inserire Auto\n(2) Inserire Furgone\n(3) Inserire Moto\n(4) Stato Garage:\n(5) Cerca con Targa:\n(6) Cerca con Proprietario\n(7) Rimuovi con Targa");
            func = Integer.parseInt(input.nextLine());
            String mod,targ,prop,tipo;
            int p,cap;
            switch (func) {
                case 1:
                    System.out.println("Inserisci Modello Auto:");
                    mod = input.nextLine();
                    System.out.println("Inserisci Targa");
                    targ = input.nextLine();
                    System.out.println("Inserisci Proprietario:");
                    prop = input.nextLine();
                    System.out.println("Inserisci Posti:");
                    p = Integer.parseInt(input.nextLine());
                    veicolo.auto a1 = new veicolo.auto(mod,targ, prop, p);
                    garage.add(a1);
                    break;
                case 2:
                    System.out.println("Inserisci Modello Furgone:");
                    mod = input.nextLine();
                    System.out.println("Inserisci Targa");
                    targ = input.nextLine();
                    System.out.println("Inserisci Proprietario:");
                    prop = input.nextLine();
                    System.out.println("Inserisci Capacità:");
                    cap = Integer.parseInt(input.nextLine());
                    veicolo.furgone f1 = new veicolo.furgone(mod,targ, prop, cap);
                    garage.add(f1);
                    break;

                case 3:
                    System.out.println("Inserisci Modello Moto:");
                    mod = input.nextLine();
                    System.out.println("Inserisci Targa");
                    targ = input.nextLine();
                    System.out.println("Inserisci Proprietario:");
                    prop = input.nextLine();
                    System.out.println("Inserisci Tipo:");
                    tipo = input.nextLine();
                    veicolo.moto m1 = new veicolo.moto(mod,targ, prop, tipo);
                    garage.add(m1);
                    break;

                case 4:
                    garage.stato();
                    break;

                case 5:

                    System.out.println("Inserisci Targa o parte di essa:");
                    targ = input.nextLine();
                    garage.ricercatarga(targ);
                    break;

                case 6:
                    System.out.println("Inserisci Prop o parte di esso:");
                    targ = input.nextLine();
                    garage.ricercaprop(targ);
                    break;

                case 7:

                    System.out.println("Insert exited veichle plate:");
                    targ = input.nextLine();
                    garage.quitveicolo(targ);
                    break;
            }
            System.out.println("Vuoi fare un'altra Operazione? (1):YES/ (2):NOT");
            int cont = Integer.parseInt(input.nextLine());
            if (cont == 2) {
                exit =1;
            }
        }
    }
}

