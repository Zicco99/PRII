import java.util.ArrayList;
/*
 * I used this class to manage data for the First Implementation
 * every kind of data is checked before inserting in this class thought CheckData.java
 */


public class Category <E extends Data> {
    private ArrayList<E> contents;
    private ArrayList<String> whitelist;
    private String category;


    public Category(String category_name) {
        this.category = category_name;
        this.contents = new ArrayList<>();
        this.whitelist = new ArrayList<>();
    }

    public String getCategory() {
        return category;
    }

    public ArrayList<E> getContents() {
        return contents;
    }

    public void setContents(ArrayList<E> contents) {
        this.contents = contents;
    }

    public void setWhitelist(ArrayList<String> whitelist) {
        this.whitelist = whitelist;
    }

    public ArrayList<String> getWhitelist() {
        return whitelist;
    }

    public void setCategory(String category) {
        this.category = category;
    }


}