package minihud.network.servux;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.google.common.collect.ImmutableList;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;

import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;

import malilib.network.ClientPacketChannelHandler;
import malilib.network.PacketUtils;
import malilib.network.message.BasePacketHandler;
import malilib.overlay.message.MessageDispatcher;
import malilib.registry.Registry;
import malilib.util.MathUtils;
import malilib.util.data.Constants;
import malilib.util.data.palette.HashMapPalette;
import malilib.util.data.palette.Palette;
import malilib.util.game.wrap.GameWrap;
import malilib.util.game.wrap.NbtWrap;
import malilib.util.nbt.NbtUtils;
import minihud.MiniHud;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;
import minihud.data.MobCapData;
import minihud.data.MobCapDataHandler;
import minihud.data.TpsDataManager;
import minihud.network.BufferReader;

public class ServuxInfoSubDataPacketHandler extends BasePacketHandler
{
    public static final ServuxInfoSubDataPacketHandler INSTANCE = new ServuxInfoSubDataPacketHandler();
    protected static final List<ResourceLocation> CHANNELS = ImmutableList.of(new ResourceLocation("servux:info_data"));

    protected final Map<String, BufferReader> allBufferReaders = new HashMap<>();
    protected final Int2ObjectOpenHashMap<BufferReader> idMappedDataReaders = new Int2ObjectOpenHashMap<>();
    protected final Set<String> currentSubscriptions = new HashSet<>();
    protected Palette<String> channelIdPalette = new HashMapPalette<>(7, this::resizePalette);
    protected boolean supportedProtocol;

    protected ServuxInfoSubDataPacketHandler()
    {
        this.registerToServer = true;
        this.registerDataHandlers();
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        MiniHud.debugLog("ServuxInfoSubDataPacketHandler#onPacketReceived() - start");

        if (this.supportedProtocol)
        {
            final int dataCount = buf.readVarInt();
            MiniHud.debugLog("ServuxInfoSubDataPacketHandler#onPacketReceived() - data count: {}", dataCount);

            for (int i = 0; i < dataCount; ++i)
            {
                int channelId = buf.readVarInt();
                BufferReader reader = this.idMappedDataReaders.get(channelId);

                if (reader == null)
                {
                    this.supportedProtocol = false;
                    MessageDispatcher.error().console().translate("minihud.message.error.info_sub.failed_receive_data", channelId);
                    break;
                }

                MiniHud.debugLog("ServuxInfoSubDataPacketHandler#onPacketReceived() - channel: {}", channelId);
                reader.readData(buf);
            }
        }
    }

    public void receiveMetadata(NBTTagCompound tag)
    {
        MiniHud.debugLog("ServuxInfoSubDataPacketHandler#receiveMetadata(), tag: {}", tag);

        if (NbtWrap.getInt(tag, "version") == 1 &&
            NbtWrap.containsList(tag, "channel_ids"))
        {
            NBTTagList listTag = NbtWrap.getList(tag, "channel_ids", Constants.NBT.TAG_STRING);
            List<String> mapping = new ArrayList<>();
            final int size = NbtWrap.getListSize(listTag);

            this.idMappedDataReaders.clear();

            for (int i = 0; i < size; ++i)
            {
                String channel = listTag.getStringTagAt(i);
                BufferReader reader = this.allBufferReaders.get(channel);

                mapping.add(channel);

                if (reader != null)
                {
                    this.idMappedDataReaders.put(i, reader);
                    this.currentSubscriptions.add(channel);
                }
            }

            if (mapping.size() > this.channelIdPalette.getMaxSize())
            {
                this.createPaletteForSize(mapping.size());
            }

            this.channelIdPalette.setMapping(mapping);
            this.supportedProtocol = true;
        }
        else
        {
            this.supportedProtocol = false;
        }
    }

    public void onLogout()
    {
        this.supportedProtocol = false;
    }

    protected int resizePalette(int bitsIn, String valueBeingAdded, Palette<String> oldPalette)
    {
        HashMapPalette<String> newPalette = new HashMapPalette<>(bitsIn, this::resizePalette);
        newPalette.setMapping(oldPalette.getMapping());
        this.channelIdPalette = newPalette;
        return newPalette.idFor(valueBeingAdded);
    }

    protected void createPaletteForSize(int minSize)
    {
        int bits = MathUtils.log2DeBruijn(minSize);
        this.channelIdPalette = new HashMapPalette<>(bits, this::resizePalette);
    }

    protected void receiveMobCapCurrentValue(MobCapData.EntityCategory type, int currentValue)
    {
        MobCapDataHandler.INSTANCE.putServerSubscribedMobCapCurrentValue(type, currentValue);
    }

    protected void receiveMobCapCapValue(MobCapData.EntityCategory type, int capValue)
    {
        MobCapDataHandler.INSTANCE.putServerSubscribedMobCapCapValue(type, capValue);
    }

    protected void registerDataHandlers()
    {
        // minecraft.status.chunk_loading.dropped_chunks.hash_size[.dimension_name]
        // minecraft.status.count.loaded.block_entities[.dimension_name]
        // minecraft.status.count.loaded.block_entities.ticking[.dimension_name]
        // minecraft.status.count.loaded.chunks[.dimension_name]
        // minecraft.status.count.loaded.entities[.dimension_name]
        // minecraft.status.count.mobcap.<capname>.val[.dimension_name]
        // minecraft.status.count.mobcap.<capname>.max[.dimension_name]
        // minecraft.status.count.scheduled_block_ticks[.dimension_name]
        // minecraft.status.performance.tps
        // minecraft.status.performance.mspt

        // 1.13+/1.14+
        // minecraft.status.count.loaded.pois[.dimension_name]
        // minecraft.status.count.scheduled_fluid_ticks[.dimension_name]
        TpsDataManager tpsManager = TpsDataManager.INSTANCE;

        this.allBufferReaders.put("minecraft.status.chunk_loading.dropped_chunks.hash_size", (buf) -> DataStorage.getInstance().setServerDroppedChunksHashSize(buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.loaded.block_entities", (buf) -> buf.readVarInt()); // TODO
        this.allBufferReaders.put("minecraft.status.count.loaded.block_entities.ticking", (buf) -> buf.readVarInt());
        this.allBufferReaders.put("minecraft.status.count.loaded.chunks", (buf) -> buf.readVarInt());
        this.allBufferReaders.put("minecraft.status.count.loaded.entities", (buf) -> buf.readVarInt());
        this.allBufferReaders.put("minecraft.status.count.scheduled_block_ticks", (buf) -> buf.readVarInt());
        this.allBufferReaders.put("minecraft.status.performance.tps",  (buf) -> tpsManager.addServerSubscribedTps(buf.readFloat()));
        this.allBufferReaders.put("minecraft.status.performance.mspt", (buf) -> tpsManager.addServerSubscribedMspt(buf.readFloat()));

        this.allBufferReaders.put("minecraft.status.count.mobcap.monster.val",        (buf) -> this.receiveMobCapCurrentValue(MobCapData.EntityCategory.MONSTER, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.monster.max",        (buf) -> this.receiveMobCapCapValue(MobCapData.EntityCategory.MONSTER, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.creature.val",       (buf) -> this.receiveMobCapCurrentValue(MobCapData.EntityCategory.CREATURE, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.creature.max",       (buf) -> this.receiveMobCapCapValue(MobCapData.EntityCategory.CREATURE, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.ambient.val",        (buf) -> this.receiveMobCapCurrentValue(MobCapData.EntityCategory.AMBIENT, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.ambient.max",        (buf) -> this.receiveMobCapCapValue(MobCapData.EntityCategory.AMBIENT, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.water_creature.val", (buf) -> this.receiveMobCapCurrentValue(MobCapData.EntityCategory.WATER, buf.readVarInt()));
        this.allBufferReaders.put("minecraft.status.count.mobcap.water_creature.max", (buf) -> this.receiveMobCapCapValue(MobCapData.EntityCategory.WATER, buf.readVarInt()));
    }

    protected NBTTagCompound channelToTag(String channel)
    {
        NBTTagCompound tag = new NBTTagCompound();
        NbtWrap.putString(tag, "channel", channel);
        return tag;
    }

    protected void updateSubscriptions(String action, Collection<String> channels)
    {
        NetHandlerPlayClient handler = GameWrap.getClient().getConnection();

        if (handler != null)
        {
            NBTTagCompound rootTag = new NBTTagCompound();
            NbtWrap.putString(rootTag, "action", action);
            NbtWrap.putTag(rootTag, "channels", NbtUtils.asListTag(channels, this::channelToTag));

            PacketUtils.sendTag(ServuxInfoSubRegistrationPacketHandler.REG_CHANNEL, rootTag, handler);
            MiniHud.debugLog("ServuxInfoSubDataPacketHandler#updateSubscriptions(), tag: {}", rootTag);
        }
    }

    public void updateSubscriptions()
    {
        ClientPacketChannelHandler handler = Registry.CLIENT_PACKET_CHANNEL_HANDLER;
        Set<String> newSubs = new HashSet<>();

        // Unsubscribe from any previous subscriptions
        Set<String> unSubs = new HashSet<>(this.currentSubscriptions);
        this.currentSubscriptions.clear();

        if (InfoLineToggle.SERVER_TPS.getBooleanValue())
        {
            newSubs.add("minecraft.status.performance.tps");
            newSubs.add("minecraft.status.performance.mspt");
        }

        if (InfoLineToggle.MOB_CAPS.getBooleanValue())
        {
            newSubs.add("minecraft.status.count.mobcap.monster.val");
            newSubs.add("minecraft.status.count.mobcap.monster.max");
            newSubs.add("minecraft.status.count.mobcap.creature.val");
            newSubs.add("minecraft.status.count.mobcap.creature.max");
            newSubs.add("minecraft.status.count.mobcap.ambient.val");
            newSubs.add("minecraft.status.count.mobcap.ambient.max");
            newSubs.add("minecraft.status.count.mobcap.water_creature.val");
            newSubs.add("minecraft.status.count.mobcap.water_creature.max");
        }

        if ((InfoLineToggle.CHUNK_UNLOAD_ORDER.getBooleanValue() ||
             RendererToggle.CHUNK_UNLOAD_BUCKET.isRendererEnabled())
            && Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue())
        {
            newSubs.add("minecraft.status.chunk_loading.dropped_chunks.hash_size");
        }

        unSubs.removeAll(newSubs);

        if (unSubs.isEmpty() == false || newSubs.isEmpty() == false)
        {
            handler.registerClientChannelHandler(INSTANCE);

            if (unSubs.isEmpty() == false)
            {
                this.updateSubscriptions("unregister", unSubs);
            }

            if (newSubs.isEmpty() == false)
            {
                this.updateSubscriptions("register", newSubs);
            }
        }

        if (newSubs.isEmpty())
        {
            handler.unregisterClientChannelHandler(INSTANCE);
        }
    }
}
