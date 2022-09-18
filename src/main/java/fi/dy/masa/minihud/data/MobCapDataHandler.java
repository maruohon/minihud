package fi.dy.masa.minihud.data;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraft.client.MinecraftClient;
import net.minecraft.text.Text;
import net.minecraft.util.Formatting;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.data.MobCapData.EntityCategory;

public class MobCapDataHandler
{
    protected static final Pattern PATTERN_CARPET_MOBCAPS = Pattern.compile("(?<hocu>[0-9-]+)/(?<hoca>-?[0-9]+),(?<pacu>[0-9-]+)/(?<paca>-?[0-9]+),(?<amcu>[0-9-]+)/(?<amca>-?[0-9]+),(?<axcu>[0-9-]+)/(?<axca>-?[0-9]+),(?<uwccu>[0-9-]+)/(?<uwcca>-?[0-9]+),(?<wccu>[0-9-]+)/(?<wcca>-?[0-9]+),(?<wacu>[0-9-]+)/(?<waca>-?[0-9]+),(?<micu>[0-9-]+)/(?<mica>-?[0-9]+)");
    protected static final EntityCategory[] ENTITY_CATEGORIES = EntityCategory.values();

    protected final MinecraftClient mc = MinecraftClient.getInstance();
    protected final MobCapData localData = new MobCapData();
    protected final MobCapData parsedServerData = new MobCapData();
    protected final MobCapData subscribedServerData = new MobCapData();
    protected long lastSyncWorldTick = -1;

    public void clear()
    {
        this.localData.clear();
        this.parsedServerData.clear();
        this.subscribedServerData.clear();
        this.lastSyncWorldTick = -1;
    }

    public boolean getHasValidData()
    {
        return this.subscribedServerData.getHasValidData() ||
                       this.parsedServerData.getHasValidData() ||
                       this.localData.getHasValidData();
    }

    public boolean shouldParsePlayerListData(long worldTick)
    {
        return this.subscribedServerData.getHasRecentValidData(worldTick) == false &&
                       this.localData.getHasRecentValidData(worldTick) == false;
    }

    public MobCapData getMobCapData()
    {
        if (this.subscribedServerData.getHasValidData())
        {
            return this.subscribedServerData;
        }
        else if (this.parsedServerData.getHasValidData())
        {
            return this.parsedServerData;
        }

        return this.localData;
    }

    public long getLastSyncedTick()
    {
        return this.lastSyncWorldTick;
    }

    public void putCarpetSubscribedMobCapCurrentValue(String name, int currentValue)
    {
        EntityCategory type = EntityCategory.fromVanillaCategoryName(name);

        if (type != null)
        {
            this.putCurrentValue(this.subscribedServerData, type, currentValue);
        }
    }

    public void putCarpetSubscribedMobCapCapValue(String name, int capValue)
    {
        EntityCategory type = EntityCategory.fromVanillaCategoryName(name);

        if (type != null)
        {
            this.putCapValue(this.subscribedServerData, type, capValue);
        }
    }

    public void putServerSubscribedMobCapValues(EntityCategory type, int currentValue, int capValue)
    {
        this.putServerSubscribedMobCapCurrentValue(type, currentValue);
        this.putServerSubscribedMobCapCapValue(type, capValue);
    }

    public void putServerSubscribedMobCapCurrentValue(EntityCategory type, int currentValue)
    {
        this.putCurrentValue(this.subscribedServerData, type, currentValue);
    }

    public void putServerSubscribedMobCapCapValue(EntityCategory type, int capValue)
    {
        this.putCapValue(this.subscribedServerData, type, capValue);
    }

    protected void putCurrentValue(MobCapData data, EntityCategory type, int currentValue)
    {
        data.setCurrentValue(type, currentValue, this.getWorldTick());
    }

    protected void putCapValue(MobCapData data, EntityCategory type, int capValue)
    {
        data.setCapValue(type, capValue, this.getWorldTick());
    }

    protected long getWorldTick()
    {
        return this.mc.world.getTime();
    }

    private void setPlayerListParsedData(EntityCategory type, int currentValue, int capValue, long worldTick)
    {
        if (this.subscribedServerData.getHasRecentValidData(worldTick) == false)
        {
            this.parsedServerData.setCurrentAndCapValues(type, currentValue, capValue, worldTick);
        }
    }

    public void updateIntegratedServerMobCaps()
    {
        /*
        if (this.mc.isIntegratedServerRunning() && this.mc.world != null)
        {
            MinecraftServer server = this.mc.getServer();
            RegistryKey<World> dim = this.mc.world.getRegistryKey();

            server.execute(() -> {
                ServerWorld world = server.getWorld(dim);
                MobCapData.Cap[] data = MobCapData.createCapArray();
                int spawnableChunks = MiscUtils.getSpawnableChunksCount(world);
                int divisor = 17 * 17;
                long worldTime = world.getTime();

                for (EntityCategory category : ENTITY_CATEGORIES)
                {
                    SpawnGroup type = category.getVanillaCategory();
                    int current = world.getEnticountEntities(type.getCreatureClass());
                    int cap = type.getCapacity() * spawnableChunks / divisor;
                    data[category.ordinal()].setCurrentAndCap(current, cap);
                }

                this.mc.execute(() -> {
                    for (EntityCategory type : ENTITY_CATEGORIES)
                    {
                        MobCapData.Cap cap = data[type.ordinal()];
                        this.localData.setCurrentAndCapValues(type, cap.getCurrent(), cap.getCap(), worldTime);
                    }
                });
            });
        }
        */
    }

    public void parsePlayerListFooterMobCapData(Text textComponent)
    {
        if (this.mc.world == null || InfoToggle.MOB_CAPS.getBooleanValue() == false)
        {
            return;
        }

        long worldTick = this.getWorldTick();

        if (this.shouldParsePlayerListData(worldTick) == false)
        {
            return;
        }

        String str = textComponent.getString();

        if (str.isEmpty() == false)
        {
            String text = Formatting.strip(str);
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_MOBCAPS.matcher(line);

                if (matcher.matches())
                {
                    try
                    {
                        int hoCu = parseMobCapValue(matcher, "hocu");
                        int hoCa = parseMobCapValue(matcher, "hoca");
                        int paCu = parseMobCapValue(matcher, "pacu");
                        int paCa = parseMobCapValue(matcher, "paca");
                        int amCu = parseMobCapValue(matcher, "amcu");
                        int amCa = parseMobCapValue(matcher, "amca");
                        int axCu = parseMobCapValue(matcher, "axcu");
                        int axCa = parseMobCapValue(matcher, "axca");
                        int uwcCu = parseMobCapValue(matcher, "uwccu");
                        int uwcCa = parseMobCapValue(matcher, "uwcca");
                        int wcCu = parseMobCapValue(matcher, "wccu");
                        int wcCa = parseMobCapValue(matcher, "wcca");
                        int waCu = parseMobCapValue(matcher, "wacu");
                        int waCa = parseMobCapValue(matcher, "waca");
                        int miCu = parseMobCapValue(matcher, "micu");
                        int miCa = parseMobCapValue(matcher, "mica");

                        this.setPlayerListParsedData(EntityCategory.MONSTER, hoCu, hoCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.CREATURE, paCu, paCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.AMBIENT, amCu, amCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.AXOLOTLS, axCu, axCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.UNDERGROUND_WATER_CREATURE, uwcCu, uwcCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.WATER_CREATURE, wcCu, wcCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.WATER_AMBIENT, waCu, waCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.MISC, miCu, miCa, worldTick);

                        break;
                    }
                    catch (NumberFormatException ignore) {}
                }
            }
        }
    }

    protected static int parseMobCapValue(Matcher matcher, String groupName)
    {
        String str = matcher.group(groupName);
        return str.equals("-") ? 0 : Integer.parseInt(str);
    }

    public String getFormattedInfoLine()
    {
        MobCapData data = this.getMobCapData();

        if (data.getHasValidData() == false)
        {
            return StringUtils.translate("minihud.info_line.mobcap.no_data");
        }

        return StringUtils.translate("minihud.info_line.mobcap.data",
                                     this.getCapString(EntityCategory.MONSTER, data),
                                     this.getCapString(EntityCategory.CREATURE, data),
                                     this.getCapString(EntityCategory.AMBIENT, data),
                                     this.getCapString(EntityCategory.AXOLOTLS, data),
                                     this.getCapString(EntityCategory.UNDERGROUND_WATER_CREATURE, data),
                                     this.getCapString(EntityCategory.WATER_CREATURE, data),
                                     this.getCapString(EntityCategory.WATER_AMBIENT, data),
                                     this.getCapString(EntityCategory.MISC, data));
    }

    private String getCapString(EntityCategory type, MobCapData data)
    {
        MobCapData.Cap capData = data.getCap(type);
        String keyStart = "minihud.info_line.mobcap.cap.";
        String key = keyStart + type.getName() + (capData.isFull() ? ".full" : ".nonfull");

        return StringUtils.translate(key, capData.getCurrent(), capData.getCap());
    }
}
