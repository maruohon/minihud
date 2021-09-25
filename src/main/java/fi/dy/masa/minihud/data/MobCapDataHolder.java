package fi.dy.masa.minihud.data;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.collect.ImmutableMap;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.WorldServer;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.util.MiscUtils;

public class MobCapDataHolder
{
    private static final Pattern PATTERN_CARPET_MOBCAPS = Pattern.compile("(?<hocu>[0-9]+)/(?<hoca>[0-9]+) (?<pacu>[0-9]+)/(?<paca>[0-9]+) (?<amcu>[0-9]+)/(?<amca>[0-9]+) (?<wacu>[0-9]+)/(?<waca>[0-9]+)");

    protected static final EnumCreatureType[] CREATURE_TYPES = EnumCreatureType.values();
    private static final ImmutableMap<EnumCreatureType, String> NAME_MAP = ImmutableMap.of(
            EnumCreatureType.MONSTER, "monster",
            EnumCreatureType.CREATURE, "creature",
            EnumCreatureType.AMBIENT, "ambient",
            EnumCreatureType.WATER_CREATURE, "water");

    private static final ImmutableMap<String, EnumCreatureType> TYPE_MAP = ImmutableMap.of(
            "monster", EnumCreatureType.MONSTER,
            "creature", EnumCreatureType.CREATURE,
            "ambient", EnumCreatureType.AMBIENT,
            "water_creature", EnumCreatureType.WATER_CREATURE);

    private final Minecraft mc = Minecraft.getMinecraft();
    private final MobCapData localData = new MobCapData();
    private final MobCapData syncedParsedData = new MobCapData();
    private final MobCapData syncedPubsubData = new MobCapData();
    private final MobCapData syncedInfoSubData = new MobCapData();
    private long lastSyncWorldTick = -1;

    public void clear()
    {
        this.localData.clear();
        this.syncedParsedData.clear();
        this.syncedPubsubData.clear();
        this.syncedInfoSubData.clear();
        this.lastSyncWorldTick = -1;
    }

    public boolean getHasValidData()
    {
        return this.syncedPubsubData.getHasValidData() ||
               this.syncedInfoSubData.getHasValidData() ||
               this.syncedParsedData.getHasValidData() ||
               this.localData.getHasValidData();
    }

    public boolean shouldParsePlayerListData()
    {
        return this.syncedPubsubData.getHasValidData() == false &&
               this.syncedInfoSubData.getHasValidData() == false &&
               this.localData.getHasValidData() == false;
    }

    public MobCapData getCapData()
    {
        if (this.syncedPubsubData.getHasValidData())
        {
            return this.syncedPubsubData;
        }
        else if (this.syncedInfoSubData.getHasValidData())
        {
            return this.syncedInfoSubData;
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

    public void handleServuxInfoSubMobCap(EnumCreatureType type, boolean isCap, int value)
    {
        if (type != null && this.mc.world != null)
        {
            this.syncedPubsubData.setCapValue(type, isCap, value, this.mc.world.getTotalWorldTime());
        }
    }

    private void setPlayerListParsedData(EnumCreatureType type, int current, int cap, long worldTick)
    {
        if (this.syncedPubsubData.getHasValidData() == false &&
            this.syncedInfoSubData.getHasValidData() == false &&
            this.mc.world != null)
        {
            this.syncedParsedData.setCapValues(type, current, cap, worldTick);
        }
    }

    public void updateIntegratedServerMobCaps()
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
                Cap[] data = MobCapData.createCapArray();

                for (EnumCreatureType type : CREATURE_TYPES)
                {
                    int current = world.countEntities(type.getCreatureClass());
                    int cap = type.getMaxNumberOfCreature() * spawnableChunks / divisor;
                    data[type.ordinal()].setCurrentAndCap(current, cap);
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

    public void parsePlayerListFooterMobCapData(ITextComponent textComponent)
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
                    catch (NumberFormatException ignore)
                    {
                    }
                }
            }
        }
    }

    public String getFormattedInfoLine()
    {
        MobCapData data = this.getCapData();

        if (data.getHasValidData() == false)
        {
            return StringUtils.translate("minihud.info_line.mobcap.no_data");
        }

        return StringUtils.translate("minihud.info_line.mobcap.data",
                this.getCapString(EnumCreatureType.MONSTER, data),
                this.getCapString(EnumCreatureType.CREATURE, data),
                this.getCapString(EnumCreatureType.WATER_CREATURE, data),
                this.getCapString(EnumCreatureType.AMBIENT, data));
    }

    private String getCapString(EnumCreatureType type, MobCapData data)
    {
        Cap capData = data.getCap(type);
        int current = capData.getCurrent();
        int cap = capData.getCap();
        boolean full = current >= cap;
        String keyStart = "minihud.info_line.mobcap.cap.";
        String capName = NAME_MAP.getOrDefault(type, "?");
        String key = keyStart + capName + (full ? ".full" : ".nonfull");

        return StringUtils.translate(key, current, cap);
    }
}
