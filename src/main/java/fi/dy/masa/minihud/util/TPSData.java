package fi.dy.masa.minihud.util;

import fi.dy.masa.malilib.gui.GuiBase;

public class TPSData
{
    private boolean hasCalculatedTPSData;
    private boolean hasSyncedTPSData;
    private boolean hasPubsubData;
    private double calculatedServerTPS;
    private double calculatedServerMSPT;
    private double syncedServerTPS;
    private double syncedServerMSPT;
    private double syncedServerTPSStaging = -1;
    private double syncedServerMSPTStaging = -1;
    private long lastSyncStagingTPS;
    private long lastSyncStagingMSPT;
    private long lastSync;

    public void clear()
    {
        this.hasCalculatedTPSData = false;
        this.hasSyncedTPSData = false;
        this.hasPubsubData = false;
        this.lastSync = -1;
    }

    public boolean getHasValidData()
    {
        return this.hasSyncedTPSData || this.hasCalculatedTPSData;
    }

    public boolean getHasSyncedData()
    {
        return this.hasSyncedTPSData;
    }

    public boolean shouldParsePlayerListData()
    {
        return this.hasPubsubData == false;
    }

    public long getLastSyncedTick()
    {
        return this.lastSync;
    }

    public void setCalculatedData(double tps, double mspt, long worldTime)
    {
        this.calculatedServerTPS = tps;
        this.calculatedServerMSPT = mspt;
        this.lastSync = worldTime;
        this.hasCalculatedTPSData = true;
    }

    public void setPlayerListParsedData(double tps, double mspt, long worldTime)
    {
        if (this.hasPubsubData == false)
        {
            this.setSyncedData(tps, mspt, worldTime);
        }
    }

    public void setIntegratedServerData(double tps, double mspt, long worldTime)
    {
        this.setSyncedData(tps, mspt, worldTime);
    }

    private void setSyncedData(double tps, double mspt, long worldTime)
    {
        this.syncedServerTPS = tps;
        this.syncedServerMSPT = mspt;
        this.lastSync = worldTime;
        this.hasSyncedTPSData = true;
    }

    public void setSyncedTPS(double tps, long worldTime)
    {
        this.syncedServerTPSStaging = tps;
        this.lastSyncStagingTPS = worldTime;
        this.checkStagingComplete();
    }

    public void setSyncedMSPT(double mspt, long worldTime)
    {
        this.syncedServerMSPTStaging = mspt;
        this.lastSyncStagingMSPT = worldTime;
        this.checkStagingComplete();
    }

    private void checkStagingComplete()
    {
        if (this.syncedServerTPSStaging >= 0 &&
            this.syncedServerMSPTStaging >= 0 &&
            Math.abs(this.lastSyncStagingTPS - this.lastSyncStagingMSPT) <= 60)
        {
            this.setSyncedData(this.syncedServerTPSStaging, this.syncedServerMSPTStaging, this.lastSyncStagingMSPT);

            this.hasPubsubData = true;
            this.syncedServerTPSStaging = -1;
            this.syncedServerMSPTStaging = -1;
            this.lastSyncStagingTPS = -1;
            this.lastSyncStagingMSPT = -1;
        }
    }

    public String getFormattedInfoLine()
    {
        if (this.getHasValidData() == false)
        {
            return "Server TPS: <no valid data>";
        }

        boolean isSynced = this.hasSyncedTPSData;
        double tps = isSynced ? this.syncedServerTPS : this.calculatedServerTPS;
        double mspt = isSynced ? this.syncedServerMSPT : this.calculatedServerMSPT;
        String rst = GuiBase.TXT_RST;
        String preTps = tps >= 20.0D ? GuiBase.TXT_GREEN : GuiBase.TXT_RED;
        String preMspt;

        // Carpet server and integrated server have actual meaningful MSPT data available
        if (isSynced)
        {
            if      (mspt <= 40) { preMspt = GuiBase.TXT_GREEN; }
            else if (mspt <= 45) { preMspt = GuiBase.TXT_YELLOW; }
            else if (mspt <= 50) { preMspt = GuiBase.TXT_GOLD; }
            else                 { preMspt = GuiBase.TXT_RED; }

            return String.format("Server TPS: %s%.1f%s MSPT: %s%.1f%s", preTps, tps, rst, preMspt, mspt, rst);
        }
        else
        {
            if (mspt <= 51) { preMspt = GuiBase.TXT_GREEN; }
            else            { preMspt = GuiBase.TXT_RED; }

            return String.format("Server TPS: %s%.1f%s (MSPT [est]: %s%.1f%s)", preTps, tps, rst, preMspt, mspt, rst);
        }
    }
}
