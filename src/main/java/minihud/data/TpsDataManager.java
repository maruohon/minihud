package minihud.data;

import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.minecraft.server.MinecraftServer;
import net.minecraft.util.text.ITextComponent;

import malilib.render.text.TextRendererUtils;
import malilib.util.MathUtils;
import malilib.util.StringUtils;
import malilib.util.game.wrap.GameUtils;

public class TpsDataManager
{
    public static final TpsDataManager INSTANCE = new TpsDataManager();
    private static final Pattern PATTERN_CARPET_TPS = Pattern.compile("TPS: (?<tps>[0-9]+[\\.,][0-9]) MSPT: (?<mspt>[0-9]+[\\.,][0-9])");

    private final TpsData calculatedData;
    private final TpsData localData;
    private final TpsData parsedServerData;
    private final TpsData subscribedData;
    private TpsData activeData;
    private long lastServerTick = -1;
    private long lastServerTimeUpdate = -1;

    public TpsDataManager()
    {
        this.calculatedData = new TpsData(this::setActiveData);
        this.localData = new TpsData(this::setActiveData);
        this.parsedServerData = new TpsData(this::setActiveData);
        this.subscribedData = new TpsData(this::setActiveData);
        this.activeData = this.subscribedData;
    }

    public void clear()
    {
        this.calculatedData.clear();
        this.localData.clear();
        this.parsedServerData.clear();
        this.subscribedData.clear();
        this.activeData = this.subscribedData;
        this.lastServerTick = -1;
        this.lastServerTimeUpdate = -1;
    }

    public boolean getHasValidData()
    {
        return this.activeData.isValid();
    }

    public void addServerSubscribedTps(double tps)
    {
        this.subscribedData.stageTps(tps, this.lastServerTick);
    }

    public void addServerSubscribedMspt(double mspt)
    {
        this.subscribedData.stageMspt(mspt, this.lastServerTick);
    }

    protected void setActiveData(TpsData data)
    {
        this.activeData = data;
    }

    public void onServerTimeUpdate(long totalWorldTime)
    {
        long currentTime = System.nanoTime();

        // Carpet server sends the TPS and MSPT values via the player list footer data,
        // and for single player the data is grabbed directly from the integrated server.
        if (this.subscribedData.isValid() == false &&
            this.localData.isValid() == false)
        {

            if (this.lastServerTick != -1 && this.lastServerTimeUpdate != -1)
            {
                long elapsedTicks = totalWorldTime - this.lastServerTick;

                if (elapsedTicks > 0)
                {
                    double mspt = ((double) (currentTime - this.lastServerTimeUpdate) / (double) elapsedTicks) / 1000000.0;
                    double tps = mspt <= 50.0 ? 20.0 : (1000.0 / mspt);
                    this.calculatedData.setValues(tps, mspt, totalWorldTime);
                }
            }
        }
        else if (totalWorldTime - this.activeData.completionTime > 60)
        {
            this.clear();
        }

        this.lastServerTick = totalWorldTime;
        this.lastServerTimeUpdate = currentTime;
    }

    public void updateIntegratedServerTps()
    {
        MinecraftServer server = GameUtils.getIntegratedServer();

        if (server != null && GameUtils.getClientWorld() != null)
        {
            double mspt = MathUtils.average(server.tickTimeArray) / 1000000.0;
            double tps = mspt <= 50.0 ? 20.0 : (1000.0 / mspt);
            this.localData.setValues(tps, mspt, GameUtils.getCurrentWorldTick());
        }
    }

    public void parsePlayerListFooterTpsData(ITextComponent textComponent)
    {
        if (this.subscribedData.isValid() == false &&
            this.localData.isValid() == false &&
            textComponent.getFormattedText().isEmpty() == false)
        {
            String text = TextRendererUtils.stripVanillaFormattingCodes(textComponent.getUnformattedText());
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_TPS.matcher(line);

                if (matcher.matches())
                {
                    try
                    {
                        this.parsedServerData.setValues(Double.parseDouble(matcher.group("tps")),
                                                        Double.parseDouble(matcher.group("mspt")),
                                                        this.lastServerTick);
                    }
                    catch (NumberFormatException ignore) {}
                }
            }
        }
    }

    public String getFormattedInfoLine()
    {
        if (this.getHasValidData() == false)
        {
            return "Server TPS: <no valid data>";
        }

        boolean isSynced = this.activeData != this.calculatedData;
        double tps = this.activeData.getTps();
        double mspt = this.activeData.getMspt();
        String tpsStr = String.format("%.1f", tps);
        String msptStr = String.format("%.1f", mspt);
        String preTps = StringUtils.translate(tps >= 20.0D ? "minihud.info_line.tps.color.tps20" : "minihud.info_line.tps.color.tps_not20");
        String preMspt;

        // Carpet server and integrated server have actual meaningful MSPT data available
        if (isSynced)
        {
            if      (mspt <= 40) { preMspt = "minihud.info_line.tps.color.mspt_below40"; }
            else if (mspt <= 45) { preMspt = "minihud.info_line.tps.color.mspt_below45"; }
            else if (mspt <= 50) { preMspt = "minihud.info_line.tps.color.mspt_below50"; }
            else                 { preMspt = "minihud.info_line.tps.color.mspt_over50"; }

            preMspt = StringUtils.translate(preMspt);
            return StringUtils.translate("minihud.info_line.tps.data_real", preTps, tpsStr, preMspt, msptStr);
        }
        else
        {
            if (mspt <= 51) { preMspt = "minihud.info_line.tps.color.mspt_below40"; }
            else            { preMspt = "minihud.info_line.tps.color.mspt_over50"; }

            preMspt = StringUtils.translate(preMspt);
            return StringUtils.translate("minihud.info_line.tps.data_estimate", preTps, tpsStr, preMspt, msptStr);
        }
    }

    public static class TpsData
    {
        protected final Consumer<TpsData> instanceListener;
        protected double tps;
        protected double mspt;
        protected double stagedTps;
        protected double stagedMspt;
        protected long tpsSetTime;
        protected long msptSetTime;
        protected long completionTime;
        protected boolean isValid;

        public TpsData(Consumer<TpsData> instanceListener)
        {
            this.instanceListener = instanceListener;
            this.clear();
        }

        public double getTps()
        {
            return this.tps;
        }

        public double getMspt()
        {
            return this.mspt;
        }

        public boolean isValid()
        {
            return this.isValid;
        }

        public boolean isValidAndRecent(long worldTick)
        {
            return this.isValid && worldTick - this.completionTime <= 40;
        }

        protected void clear()
        {
            this.isValid = false;
            this.tpsSetTime = -1L;
            this.msptSetTime = -1L;
            this.stagedTps = -1.0;
            this.stagedMspt = -1.0;
        }

        protected void setValues(double tps, double mspt, long worldTick)
        {
            this.tps = tps;
            this.mspt = mspt;
            this.completionTime = worldTick;
            this.isValid = true;
            this.instanceListener.accept(this);
        }

        protected void stageTps(double tps, long worldTick)
        {
            this.stagedTps = tps;
            this.tpsSetTime = worldTick;
            this.checkStaging(worldTick);
        }

        protected void stageMspt(double mspt, long worldTick)
        {
            this.stagedMspt = mspt;
            this.msptSetTime = worldTick;
            this.checkStaging(worldTick);
        }

        protected void checkStaging(long worldTick)
        {
            if (this.stagedTps != -1.0 && this.stagedMspt != -1.0 &&
                Math.abs(this.tpsSetTime - this.msptSetTime) <= 40)
            {
                double tps = this.stagedTps;
                double mspt = this.stagedMspt;
                this.clear();
                this.setValues(tps, mspt, worldTick);
            }
        }
    }
}
