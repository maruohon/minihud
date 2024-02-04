package minihud.data;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;

import malilib.render.text.TextRendererUtils;
import malilib.util.StringUtils;
import malilib.util.game.wrap.WorldWrap;
import malilib.util.game.wrap.GameWrap;
import minihud.data.MobCapData.EntityCategory;
import minihud.util.MiscUtils;

public class MobCapDataHandler
{
    protected static final Pattern PATTERN_CARPET_MOBCAPS = Pattern.compile("(?<hocu>[0-9]+)/(?<hoca>[0-9]+) (?<pacu>[0-9]+)/(?<paca>[0-9]+) (?<amcu>[0-9]+)/(?<amca>[0-9]+) (?<wacu>[0-9]+)/(?<waca>[0-9]+)");
    protected static final EntityCategory[] ENTITY_CATEGORIES = EntityCategory.values();

    // Needs to be after the above initializations
    public static final MobCapDataHandler INSTANCE = new MobCapDataHandler();

    protected final Minecraft mc = GameWrap.getClient();
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
        return GameWrap.getCurrentWorldTick();
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
        World clientWorld = GameWrap.getClientWorld();

        if (GameWrap.isSinglePlayer() && clientWorld != null)
        {
            MinecraftServer server = GameWrap.getIntegratedServer();
            int dim = WorldWrap.getDimensionId(clientWorld);

            server.addScheduledTask(() -> {
                WorldServer world = server.getWorld(dim);
                int spawnableChunks = MiscUtils.getSpawnableChunksCount(world);
                int divisor = 17 * 17;
                long worldTime = world.getTotalWorldTime();
                MobCapData.Cap[] data = MobCapData.createCapArray();

                for (EntityCategory category : ENTITY_CATEGORIES)
                {
                    EnumCreatureType type = category.getVanillaCategory();
                    int current = world.countEntities(type.getCreatureClass());
                    int cap = type.getMaxNumberOfCreature() * spawnableChunks / divisor;
                    data[category.ordinal()].setCurrentAndCap(current, cap);
                }

                this.mc.addScheduledTask(() -> {
                    for (EntityCategory type : ENTITY_CATEGORIES)
                    {
                        MobCapData.Cap cap = data[type.ordinal()];
                        this.localData.setCurrentAndCapValues(type, cap.getCurrent(), cap.getCap(), worldTime);
                    }
                });
            });
        }
    }

    public void parsePlayerListFooterMobCapData(ITextComponent textComponent)
    {
        if (GameWrap.getClientWorld() == null)
        {
            return;
        }

        long worldTick = this.getWorldTick();

        if (this.shouldParsePlayerListData(worldTick) &&
            textComponent.getFormattedText().isEmpty() == false)
        {
            String text = TextRendererUtils.stripVanillaFormattingCodes(textComponent.getUnformattedText());
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_MOBCAPS.matcher(line);

                if (matcher.matches())
                {
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

                        this.setPlayerListParsedData(EntityCategory.MONSTER, hoCu, hoCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.CREATURE, paCu, paCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.AMBIENT, amCu, amCa, worldTick);
                        this.setPlayerListParsedData(EntityCategory.WATER, waCu, waCa, worldTick);

                        break;
                    }
                    catch (NumberFormatException ignore) {}
                }
            }
        }
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
                this.getCapString(EntityCategory.WATER, data),
                this.getCapString(EntityCategory.AMBIENT, data));
    }

    private String getCapString(EntityCategory type, MobCapData data)
    {
        MobCapData.Cap capData = data.getCap(type);
        String keyStart = "minihud.info_line.mobcap.cap.";
        String key = keyStart + type.getName() + (capData.isFull() ? ".full" : ".nonfull");

        return StringUtils.translate(key, capData.getCurrent(), capData.getCap());
    }
}
