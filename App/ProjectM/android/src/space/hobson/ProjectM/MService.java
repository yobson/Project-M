package space.hobson.ProjectM;

import android.util.Log;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import org.qtproject.qt5.android.bindings.QtService;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.Notification.Builder;
import android.app.NotificationChannel;
import space.hobson.ProjectM.R;

public class MService extends QtService
{
    public static void startMService(Context ctx) {
        Log.i("ProjectM", "SERVICE :: startMService called");
        ctx.startService(new Intent(ctx, MService.class));
    }

    Context mContext;
    NotificationManager mNotificationManager;
@Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.i("ProjectM", "Somehow, this code is running");
        Intent activityIntent = new Intent(this, MService.class);
        activityIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, activityIntent, PendingIntent.FLAG_UPDATE_CURRENT);
        this.mContext = this;
            String CHANNEL_ID = "project_m_01";
            String name = "project_m_can";
            String description = "Notification service for Project M";
            int importance = NotificationManager.IMPORTANCE_MIN;
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name, importance);
            channel.setDescription(description);
            // Register the channel with the system; you can't change the importance
            // or other notification behaviors after this
            mNotificationManager = mContext.getSystemService(NotificationManager.class);
            mNotificationManager.createNotificationChannel(channel);

            Notification.Builder builder = new Notification.Builder(mContext, CHANNEL_ID)
            .setContentTitle("Project M")
            .setContentText("Running projects in background")
            .setContentIntent(pendingIntent)
            .setPriority(Notification.PRIORITY_MIN)
            .setSmallIcon(R.drawable.notification_icon)
            //.setLargeIcon(BitmapFactory.decodeResource(getResources(), R.drawable.ic_launcher))
            ;
        Notification n;

        startForeground(123, builder.build());

        return Service.START_STICKY;
    }
@Override
   public void onDestroy() {
       System.exit(0);  //Goodbye cruel world
   }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
