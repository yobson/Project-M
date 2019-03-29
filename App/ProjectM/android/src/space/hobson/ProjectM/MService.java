package space.hobson.ProjectM;

import org.qtproject.qt5.android.bindings.QtService;
import android.util.Log;
import android.content.Context;
import android.content.Intent;

public class MService extends QtService
{
    public static void startMService(Context ctx) {
        Log.i("ProjectM", "SERVICE :: startMService called");
        ctx.startService(new Intent(ctx, MService.class));
    }
}
