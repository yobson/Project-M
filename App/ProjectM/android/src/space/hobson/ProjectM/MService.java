package space.hobson.ProjectM;

import android.content.Context;
import android.content.Intent;
import org.qtproject.qt5.android.bindings.QtService;
import android.util.Log;

public class MService extends QtService
{
    public static void startMService(Context ctx) {
        Log.i("ProjectM", "SERVICE :: startMService called");
        ctx.startService(new Intent(ctx, MService.class));
    }
}