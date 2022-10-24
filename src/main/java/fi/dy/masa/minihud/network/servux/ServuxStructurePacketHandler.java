package fi.dy.masa.minihud.network.servux;

import java.util.List;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;

import malilib.network.message.BasePacketHandler;
import malilib.util.game.wrap.NbtWrap;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.data.structure.StructureData;
import fi.dy.masa.minihud.data.structure.StructureStorage;
import fi.dy.masa.minihud.data.structure.StructureType;

public class ServuxStructurePacketHandler extends BasePacketHandler
{
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(new ResourceLocation("servux:structure"));
    public static final ServuxStructurePacketHandler INSTANCE = new ServuxStructurePacketHandler();

    //private static final int SERVUX_PACKET_S2C_METADATA = 1;
    private static final int SERVUX_PACKET_S2C_STRUCTURE_DATA = 2;

    private ServuxStructurePacketHandler()
    {
        this.registerToServer = true;
        this.usePacketSplitter = true;
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        try
        {
            buf.readerIndex(0);
            int type = buf.readVarInt();

            if (type == SERVUX_PACKET_S2C_STRUCTURE_DATA)
            {
                NBTTagCompound tag = buf.readCompoundTag();
                StructureStorage.INSTANCE.addStructureDataFromServer(this.readStructureDataServuxV1(tag));
            }
            /*
            else if (type == SERVUX_PACKET_S2C_METADATA)
            {
            }
            */

            buf.readerIndex(0);
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("Failed to read structure data from Servux mod packet", e);
        }
    }

    private ArrayListMultimap<StructureType, StructureData> readStructureDataServuxV1(NBTTagCompound nbt)
    {
        ArrayListMultimap<StructureType, StructureData> map = ArrayListMultimap.create();
        NBTTagList tagList = NbtWrap.getListOfCompounds(nbt, "Structures");
        final int size = NbtWrap.getListSize(tagList);

        for (int i = 0; i < size; ++i)
        {
            NBTTagCompound tag = NbtWrap.getCompoundAt(tagList, i);
            String id = NbtWrap.getString(tag, "id");
            StructureType type = StructureType.fromStructureId(id);
            StructureData data = StructureData.fromTag(tag);

            if (type != null && data != null)
            {
                map.put(type, data);
            }
        }

        MiniHUD.debugLog("Structure data from Servux server, structure count = {}", map.size());

        return map;
    }
}
