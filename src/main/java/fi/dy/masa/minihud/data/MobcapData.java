package fi.dy.masa.minihud.data;

import java.util.Arrays;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.collect.ImmutableMap;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.WorldServer;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.util.MathUtils;
import fi.dy.masa.minihud.util.MiscUtils;

public class MobcapData
{
    private static final Pattern PATTERN_CARPET_MOBCAPS = Pattern.compile("(?<hocu>[0-9]+)/(?<hoca>[0-9]+) (?<pacu>[0-9]+)/(?<paca>[0-9]+) (?<amcu>[0-9]+)/(?<amca>[0-9]+) (?<wacu>[0-9]+)/(?<waca>[0-9]+)");

    private static final EnumCreatureType[] CREATURE_TYPES = EnumCreatureType.values();
    private static final ImmutableMap<String, EnumCreatureType> TYPE_MAP = ImmutableMap.of(
            EnumCreatureType.MONSTER.name().toLowerCase(Locale.ROOT), EnumCreatureType.MONSTER,
            EnumCreatureType.CREATURE.name().toLowerCase(Locale.ROOT), EnumCreatureType.CREATURE,
            EnumCreatureType.AMBIENT.name().toLowerCase(Locale.ROOT), EnumCreatureType.AMBIENT,
            EnumCreatureType.WATER_CREATURE.name().toLowerCase(Locale.ROOT), EnumCreatureType.WATER_CREATURE);

    private final Minecraft mc = Minecraft.getMinecraft();
    private final CapData localData = new CapData();
    private final CapData syncedParsedData = new CapData();
    private final CapData syncedPubsubData = new CapData();
    private long lastSyncWorldTick = -1;

    public static class Cap
    {
        private int current;
        private int cap;

        public int getCurrent()
        {
            return this.current;
        }

        public int getCap()
        {
            return this.cap;
        }

        public void setCurrent(int current)
        {
            this.current = current;
        }

        public void setCap(int cap)
        {
            this.cap = cap;
        }

        public void setValues(int current, int cap)
        {
            this.current = current;
            this.cap = cap;
        }
    }

    public static class CapData
    {
        private final Cap[] data = createCapArray();
        private final Cap[] stagingData = createCapArray();
        private final boolean[] dataValid = new boolean[CREATURE_TYPES.length];
        private final long[] worldTicks = new long[CREATURE_TYPES.length];
        private boolean hasValidData;

        public static Cap[] createCapArray()
        {
            Cap[] data = new Cap[CREATURE_TYPES.length];

            for (int i = 0; i < data.length; ++i)
            {
                data[i] = new Cap();
            }

            return data;
        }

        public void clear()
        {
            this.clearStaging();
            this.hasValidData = false;
        }

        private void clearStaging()
        {
            for (int i = 0; i < this.stagingData.length; ++i)
            {
                this.stagingData[i].setValues(-1, -1);
            }

            Arrays.fill(this.dataValid, false);
            Arrays.fill(this.worldTicks, -1);
        }

        public void setCapValue(EnumCreatureType type, boolean isCap, int value, long worldTick)
        {
            int index = type.ordinal();
            Cap cap = this.stagingData[index];

            if (isCap)
            {
                cap.setCap(value);
            }
            else
            {
                cap.setCurrent(value);
            }

            this.checkCapComplete(index, cap, worldTick);
        }

        private void checkCapComplete(int index, Cap cap, long worldTick)
        {
            if (cap.getCurrent() >= 0 && cap.getCap() >= 0)
            {
                this.dataValid[index] = true;
                this.worldTicks[index] = worldTick;

                this.checkStagingComplete();
            }
        }

        public void setCapValues(EnumCreatureType type, int current, int cap, long worldTick)
        {
            int index = type.ordinal();

            this.stagingData[index].setValues(current, cap);
            this.dataValid[index] = true;
            this.worldTicks[index] = worldTick;

            this.checkStagingComplete();
        }

        public boolean getHasValidData()
        {
            return this.hasValidData;
        }

        public Cap getCap(EnumCreatureType type)
        {
            return this.data[type.ordinal()];
        }

        private void checkStagingComplete()
        {
            for (int i = 0; i < this.dataValid.length; ++i)
            {
                if (this.dataValid[i] == false)
                {
                    return;
                }
            }

            long min = MathUtils.getMinValue(this.worldTicks);
            long max = MathUtils.getMaxValue(this.worldTicks);

            // Require all the values to have been received within 60 ticks
            // of each other for the data set to be considered valid
            if (max - min <= 60)
            {
                for (int i = 0; i < this.stagingData.length; ++i)
                {
                    Cap cap = this.stagingData[i];
                    this.data[i].setValues(cap.getCurrent(), cap.getCap());
                }

                this.clearStaging();
            }

            this.hasValidData = true;
        }
    }

    public void clear()
    {
        this.localData.clear();
        this.syncedParsedData.clear();
        this.syncedPubsubData.clear();
        this.lastSyncWorldTick = -1;
    }

    public boolean getHasValidData()
    {
        return this.syncedPubsubData.getHasValidData() ||
               this.syncedParsedData.getHasValidData() ||
               this.localData.getHasValidData();
    }

    public boolean shouldParsePlayerListData()
    {
        return this.syncedPubsubData.getHasValidData() == false &&
               this.localData.getHasValidData() == false;
    }

    public CapData getCapData()
    {
        if (this.syncedPubsubData.getHasValidData())
        {
            return this.syncedPubsubData;
        }
        else if (this.syncedParsedData.getHasValidData())
        {
            return this.syncedParsedData;
        }

        return this.localData;
    }

    public long getLastSyncedTick()
    {
        return this.lastSyncWorldTick;
    }

    public void handleCarpetServerPubsubMobcap(String name, boolean isCap, int value)
    {
        EnumCreatureType type = TYPE_MAP.get(name);

        if (type != null && this.mc.world != null)
        {
            this.syncedPubsubData.setCapValue(type, isCap, value, this.mc.world.getTotalWorldTime());
        }
    }

    private void setPlayerListParsedData(EnumCreatureType type, int current, int cap, long worldTick)
    {
        if (this.syncedPubsubData.getHasValidData() == false && this.mc.world != null)
        {
            this.syncedParsedData.setCapValues(type, current, cap, worldTick);
        }
    }

    public void updateIntegratedServerMobcaps()
    {
        if (this.mc.isSingleplayer() && this.mc.world != null)
        {
            MinecraftServer server = this.mc.getIntegratedServer();
            int dim = this.mc.world.provider.getDimensionType().getId();

            server.addScheduledTask(() -> {
                WorldServer world = server.getWorld(dim);
                int spawnableChunks = MiscUtils.getSpawnableChunksCount(world);
                int divisor = 17 * 17;
                long worldTime = world.getTotalWorldTime();
                Cap[] data = CapData.createCapArray();

                for (EnumCreatureType type : CREATURE_TYPES)
                {
                    int current = world.countEntities(type.getCreatureClass());
                    int cap = type.getMaxNumberOfCreature() * spawnableChunks / divisor;
                    data[type.ordinal()].setValues(current, cap);
                }

                this.mc.addScheduledTask(() -> {
                    for (EnumCreatureType type : CREATURE_TYPES)
                    {
                        Cap cap = data[type.ordinal()];
                        this.localData.setCapValues(type, cap.getCurrent(), cap.getCap(), worldTime);
                    }
                });
            });
        }
    }

    public void parsePlayerListFooterMobcapData(ITextComponent textComponent)
    {
        if (this.shouldParsePlayerListData() && this.mc.world != null &&
            textComponent.getFormattedText().isEmpty() == false)
        {
            String text = TextFormatting.getTextWithoutFormattingCodes(textComponent.getUnformattedText());
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_MOBCAPS.matcher(line);

                if (matcher.matches())
                {
                    long worldTick = this.mc.world.getTotalWorldTime();

                    try
                    {
                        int hoCu = Integer.parseInt(matcher.group("hocu"));
                        int hoCa = Integer.parseInt(matcher.group("hoca"));
                        int paCu = Integer.parseInt(matcher.group("pacu"));
                        int paCa = Integer.parseInt(matcher.group("paca"));
                        int amCu = Integer.parseInt(matcher.group("amcu"));
                        int amCa = Integer.parseInt(matcher.group("amca"));
                        int waCu = Integer.parseInt(matcher.group("wacu"));
                        int waCa = Integer.parseInt(matcher.group("waca"));

                        this.setPlayerListParsedData(EnumCreatureType.MONSTER, hoCu, hoCa, worldTick);
                        this.setPlayerListParsedData(EnumCreatureType.CREATURE, paCu, paCa, worldTick);
                        this.setPlayerListParsedData(EnumCreatureType.AMBIENT, amCu, amCa, worldTick);
                        this.setPlayerListParsedData(EnumCreatureType.WATER_CREATURE, waCu, waCa, worldTick);

                        break;
                    }
                    catch (NumberFormatException e)
                    {
                    }
                }
            }
        }
    }

    public String getFormattedInfoLine()
    {
        CapData data = this.getCapData();

        if (data.getHasValidData() == false)
        {
            return "Mob caps: <no valid data>";
        }

        return String.format("Mob caps: Ho: %s Pa: %s Wa: %s Am: %s",
                this.getCapString(data.getCap(EnumCreatureType.MONSTER), BaseScreen.TXT_RED),
                this.getCapString(data.getCap(EnumCreatureType.CREATURE), BaseScreen.TXT_GREEN),
                this.getCapString(data.getCap(EnumCreatureType.WATER_CREATURE), BaseScreen.TXT_BLUE),
                this.getCapString(data.getCap(EnumCreatureType.AMBIENT), BaseScreen.TXT_GRAY));
    }

    private String getCapString(Cap cap, String color)
    {
        int current = cap.getCurrent();
        int mobcap = cap.getCap();
        String rst = BaseScreen.TXT_RST;
        String pre = current >= mobcap ? color + BaseScreen.TXT_BOLD : color;

        return String.format("%s%d%s / %d%s", pre, current, color, mobcap, rst);
    }
}
