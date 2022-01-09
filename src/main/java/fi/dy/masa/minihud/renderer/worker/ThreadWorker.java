package fi.dy.masa.minihud.renderer.worker;

import net.minecraft.client.MinecraftClient;
import net.minecraft.util.crash.CrashReport;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.util.DataStorage;

public class ThreadWorker implements Runnable
{
    private boolean shouldRun = true;

    @Override
    public void run()
    {
        while (this.shouldRun)
        {
            try
            {
                ChunkTask task = DataStorage.INSTANCE.getNextTask();

                if (task != null)
                {
                    this.processTask(task.task);
                }
            }
            catch (InterruptedException e)
            {
                MiniHUD.logger.debug("Stopping worker thread due to an interrupt");
                return;
            }
            catch (Throwable throwable)
            {
                CrashReport crashreport = CrashReport.create(throwable, "MiniHUD worker thread");
                MinecraftClient.getInstance().setCrashReportSupplier(() -> MinecraftClient.getInstance().addDetailsToCrashReport(crashreport));
                return;
            }
        }
    }

    public void stopThread()
    {
        this.shouldRun = false;
    }

    protected void processTask(final Runnable task) throws InterruptedException
    {
        task.run();
    }
}
