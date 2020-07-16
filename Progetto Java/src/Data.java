import Execptions.DataNotFoundException;
import Execptions.DataNotValidExeption;
import Execptions.DuplicateException;
import Execptions.UserNotValidExeption;

import java.util.ArrayList;

   public class Data implements Comparable<Data> {
        private ArrayList<String> people;           // Users that liked this element//
        private String txt;
        private String category;
        private String owner;
        private int likes;


        public Data(String own, String txt) throws UserNotValidExeption, DataNotValidExeption {
            CheckData.idchecker(own);                    //Checks if user is valid , otherwise throws exception
            CheckData.textchecker(txt);                    //Checks if txt is valid , otherwise throws exception
            this.owner = own;
            this.txt = txt;
            this.likes = 0;
            this.people = new ArrayList<String>();        //initializes the list
        }

        // Sort by likes , by category , by name
        public int compareTo(Data target) {
            if(this.equals(target)) return 0;      // return 0 if the instances are equal
            if(this.likes == target.nlikes()) {     // same number of likes
                if(this.getCategory() != null && target.getCategory() != null ) {

                    if(this.category.equals(target.getCategory()))
                        return this.txt.compareTo(target.getTxt());       //If these 2 data objects have a category , compare them by
                    //category , otherwise by text;
                    return this.category.compareTo(target.getCategory());
                } else {
                    return this.txt.compareTo(target.getTxt());
                }
            }
            return target.nlikes() - this.likes;
        }

        void addLike(String user) throws DuplicateException, DataNotValidExeption {
            CheckData.idchecker(user);                    //Checks if user is valid , otherwise throws exception
            CheckData.textchecker(txt);                    //Checks if txt is valid , otherwise throws exception
            if(this.people.contains(user)) throw new DuplicateException("Already liked by" + user);
            this.people.add(user);
            this.likes++;
        }

        public void removeLike(String user) throws DataNotFoundException, DataNotValidExeption {
            CheckData.idchecker(user);
            if(!this.people.contains(user)) throw new DataNotFoundException(user + "didn't like this yet");
            this.people.remove(user);
            this.likes--;
        }


        private Data (Data e) {
            this.owner = e.WhoWrote();
            this.txt = e.getTxt();
            this.category = e.getCategory();
            this.likes = e.nlikes();
            this.people = e.getLikes();
        }

        @Override
        public Data clone () {
            return new Data (this);
        }

        public String WhoWrote() {
            return owner;
        }

        private String getTxt() {
            return txt;
        }

        private String getCategory() {
            return category;
        }

        void setCategory(String category) {
            this.category = category;
        }

        private int nlikes() {
            return likes;
        }

        private ArrayList<String> getLikes() {
            return new ArrayList<String>(this.people);
        }

        public String display() {
            if(this.getCategory() == null)
                return  this.WhoWrote() + " : " + this.getTxt() + " ( " + this.nlikes() + "Likes )\n";
            return "In the category "+ this.getCategory() + " : " + this.getTxt() + "\n( " + this.nlikes() + " Likes )\n";
        }


       public Object getAuthor() {
            return this.owner;
       }
   }
