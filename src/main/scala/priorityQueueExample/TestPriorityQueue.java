package priorityQueueExample;

import java.util.PriorityQueue;

/**
 * Created by Espen on 04.11.2016.
 */
public class TestPriorityQueue {
    public static void main(String[] args) {
        System.out.println("Hello world");
        PriorityQueue priorityQueue = new PriorityQueue(new IntComparator());
        priorityQueue.add(5);
        priorityQueue.add(7);
        priorityQueue.add(1);
        priorityQueue.add(10);
        priorityQueue.add(8);
        while(priorityQueue.size() > 0){
            System.out.println(priorityQueue.remove());
        }
    }
}
