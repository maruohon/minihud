package minihud.network.carpet;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import io.netty.buffer.Unpooled;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;

import malilib.network.PacketSplitter;
import malilib.network.message.BasePacketHandler;
import malilib.util.data.Constants;
import malilib.util.game.wrap.GameWrap;
import malilib.util.game.wrap.NbtWrap;
import malilib.util.position.IntBoundingBox;
import minihud.MiniHud;
import minihud.data.DataStorage;
import minihud.data.structure.StructureData;
import minihud.data.structure.StructureStorage;
import minihud.data.structure.StructureType;

public class CarpetStructurePacketHandler extends BasePacketHandler
{
    public static final ResourceLocation CHANNEL = new ResourceLocation("carpet:client");
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(CHANNEL);
    public static final CarpetStructurePacketHandler INSTANCE = new CarpetStructurePacketHandler();

    public static final int CARPET_ID_BOUNDINGBOX_MARKERS = 3;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START = 7;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS = 8;

    public static final int CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX = 0;
    public static final int CARPET_STRUCTURE_ID_END_CITY = 1;
    public static final int CARPET_STRUCTURE_ID_FORTRESS = 2;
    public static final int CARPET_STRUCTURE_ID_TEMPLE = 3;
    public static final int CARPET_STRUCTURE_ID_VILLAGE = 4;
    public static final int CARPET_STRUCTURE_ID_STRONGHOLD = 5;
    public static final int CARPET_STRUCTURE_ID_MINESHAFT = 6;
    public static final int CARPET_STRUCTURE_ID_MONUMENT = 7;
    public static final int CARPET_STRUCTURE_ID_MANSION = 8;

    private static final CarpetBoxReader CARPET_BOX_READER = new CarpetBoxReader();

    private CarpetStructurePacketHandler()
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

            if (buf.readerIndex() < buf.writerIndex() - 4)
            {
                int type = buf.readInt();
                MiniHud.debugLog("StructureStorage#updateStructureDataFromCarpetServer(), packet type = {}", type);

                if (type == CARPET_ID_BOUNDINGBOX_MARKERS)
                {
                    NBTTagCompound tag = buf.readCompoundTag();
                    StructureStorage.INSTANCE.addStructureDataFromServer(readStructureDataCarpetAllBoxes(tag));
                }
                else if (type == CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START)
                {
                    readSplitStructuresHeader(buf);
                }
                else if (type == CARPET_ID_LARGE_BOUNDINGBOX_MARKERS)
                {
                    readSplitStructureBoxes(buf);
                }
            }

            buf.readerIndex(0);
        }
        catch (Exception e)
        {
            MiniHud.LOGGER.warn("Failed to read structure data from Carpet mod packet", e);
        }
    }

    public void sendBoundingBoxRequest()
    {
        PacketBuffer buf = new PacketBuffer(Unpooled.buffer());
        buf.writeInt(CARPET_ID_BOUNDINGBOX_MARKERS);
        PacketSplitter.send(CHANNEL, buf, GameWrap.getNetworkConnection());
    }

    private static ArrayListMultimap<StructureType, StructureData> readStructureDataCarpetAllBoxes(NBTTagCompound tag)
    {
        ArrayListMultimap<StructureType, StructureData> map = ArrayListMultimap.create();
        NBTTagList tagList = NbtWrap.getList(tag, "Boxes", Constants.NBT.TAG_LIST);
        MiniHud.debugLog("StructureStorage#readStructureDataCarpetAll() - tag count: {}", tagList.tagCount());

        List<NBTTagCompound> tags = new ArrayList<>();
        final int size = NbtWrap.getListSize(tagList);

        if (NbtWrap.containsLong(tag, "Seed"))
        {
            DataStorage.getInstance().setWorldSeed(NbtWrap.getLong(tag, "Seed"));
        }

        for (int listNum = 0; listNum < size; ++listNum)
        {
            NBTTagList innerList = (NBTTagList) tagList.get(listNum);
            final int innerSize = NbtWrap.getListSize(innerList);

            for (int i = 0; i < innerSize; ++i)
            {
                tags.add(NbtWrap.getCompoundAt(innerList, i));
            }
        }

        //System.out.printf("SD - readStructureDataCarpetAllBoxes, list: %d\n", tags.size());
        readStructureDataCarpetAllBoxes(map, tags);
        MiniHud.debugLog("Structure data from Carpet server (all), structure count = {}", map.size());

        return map;
    }

    private static void readStructureDataCarpetAllBoxes(ArrayListMultimap<StructureType, StructureData> map, List<NBTTagCompound> tags)
    {
        ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
        IntBoundingBox bbMain = null;
        StructureType type = null;
        int componentBoxes = 0;

        for (int i = 0; i < tags.size(); ++i)
        {
            NBTTagCompound tag = tags.get(i);
            int id = NbtWrap.getInt(tag, "type");

            // Carpet puts the main box as the first entry in the same list of compound tags
            // Only the subsequent component boxes will have the structure type ID
            if (id == CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX)
            {
                // The beginning of another structure
                if (type != null && bbMain != null)
                {
                    map.put(type, new StructureData(bbMain, builder.build()));
                    builder = ImmutableList.builder();
                    type = null;
                }

                if (tags.size() > i + 1)
                {
                    bbMain = IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb"));
                    id = NbtWrap.getInt(tags.get(i + 1), "type");
                    type = getTypeFromCarpetId(id);
                }
                MiniHud.debugLog("readStructureDataCarpetAllBoxes(): Enclosing box, id = {}", id);
            }
            // Don't add the component boxes of unknown/unsupported structure types to the builder
            else if (type != null)
            {
                builder.add(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb")));
                ++componentBoxes;
                MiniHud.debugLog("readStructureDataCarpetAllBoxes(): componentBoxes currently: {}", componentBoxes);
            }
        }

        if (componentBoxes > 0 && type != null && bbMain != null)
        {
            map.put(type, new StructureData(bbMain, builder.build()));
        }
    }

    private static void readSplitStructuresHeader(PacketBuffer buf)
    {
        CarpetBoxReader reader = CARPET_BOX_READER;
        reader.reset();

        try
        {
            NBTTagCompound tag = buf.readCompoundTag();
            int boxCount = buf.readVarInt();

            reader.expectedStructures = boxCount;
            MiniHud.debugLog("Structure data header received from Carpet server, expecting {} boxes", boxCount);

            if (NbtWrap.containsLong(tag, "Seed"))
            {
                DataStorage.getInstance().setWorldSeed(NbtWrap.getLong(tag, "Seed"));
            }
        }
        catch (Exception e)
        {
            MiniHud.LOGGER.warn("Failed to read structure data from Carpet server (split boxes header)", e);
        }
    }

    private static void readSplitStructureBoxes(PacketBuffer buf)
    {
        try
        {
            List<NBTTagCompound> tags = new ArrayList<>();
            int boxCount = buf.readByte();

            for (int i = 0; i < boxCount; ++i)
            {
                tags.add(buf.readCompoundTag());
            }

            readSplitStructureBoxes(tags);
        }
        catch (Exception e)
        {
            MiniHud.LOGGER.warn("Failed to read structure data from Carpet server", e);
        }
    }

    private static void readSplitStructureBoxes(List<NBTTagCompound> tags)
    {
        for (NBTTagCompound tag : tags)
        {
            readSplitStructureBox(tag);
        }

        MiniHud.debugLog("Structure data received from Carpet server (split boxes), received {} boxes", tags.size());
    }

    private static void readSplitStructureBox(NBTTagCompound tag)
    {
        CarpetBoxReader reader = CARPET_BOX_READER;
        int id = NbtWrap.getInt(tag, "type");

        reader.seenStructures++;

        if (id == CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX)
        {
            if (reader.type != null &&
                reader.bbMain != null &&
                reader.componentBoxes > 0)
            {
                reader.map.put(reader.type, new StructureData(reader.bbMain, reader.componentsBuilder.build()));
            }

            IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb"));
            reader.bbMain = bb;
            reader.readTypeFromNextBox = true;
            reader.type = null;
            reader.componentsBuilder = ImmutableList.builder();
        }
        else
        {
            reader.componentBoxes++;

            if (reader.readTypeFromNextBox)
            {
                reader.type = getTypeFromCarpetId(id);
                reader.readTypeFromNextBox = false;
            }

            if (reader.componentsBuilder != null)
            {
                IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb"));
                reader.componentsBuilder.add(bb);
            }
        }

        if (reader.seenStructures >= reader.expectedStructures)
        {
            if (reader.type != null &&
                reader.bbMain != null &&
                reader.componentBoxes > 0)
            {
                reader.map.put(reader.type, new StructureData(reader.bbMain, reader.componentsBuilder.build()));
            }

            MiniHud.debugLog("Structure data from Carpet server (split data), structure count = {}", reader.map.size());
            StructureStorage.INSTANCE.addStructureDataFromServer(reader.map);
            reader.reset();
        }
    }

    @Nullable
    private static StructureType getTypeFromCarpetId(int id)
    {
        switch (id)
        {
            case CARPET_STRUCTURE_ID_END_CITY:      return StructureType.END_CITY;
            case CARPET_STRUCTURE_ID_FORTRESS:      return StructureType.NETHER_FORTRESS;
            case CARPET_STRUCTURE_ID_MANSION:       return StructureType.MANSION;
            case CARPET_STRUCTURE_ID_MONUMENT:      return StructureType.OCEAN_MONUMENT;
            case CARPET_STRUCTURE_ID_MINESHAFT:     return StructureType.MINESHAFT;
            case CARPET_STRUCTURE_ID_STRONGHOLD:    return StructureType.STRONGHOLD;
            case CARPET_STRUCTURE_ID_TEMPLE:        return StructureType.WITCH_HUT;
            case CARPET_STRUCTURE_ID_VILLAGE:       return StructureType.VILLAGE;
        }

        return null;
    }

    private static class CarpetBoxReader
    {
        protected StructureType type;
        protected IntBoundingBox bbMain;
        protected ImmutableList.Builder<IntBoundingBox> componentsBuilder;
        protected ArrayListMultimap<StructureType, StructureData> map = ArrayListMultimap.create();
        protected boolean readTypeFromNextBox;
        protected int expectedStructures = -1;
        protected int seenStructures;
        protected int componentBoxes;

        protected void reset()
        {
            this.map = ArrayListMultimap.create();
            this.type = null;
            this.bbMain = null;
            this.componentsBuilder = null;
            this.readTypeFromNextBox = false;
            this.componentBoxes = 0;
            this.expectedStructures = -1;
            this.seenStructures = 0;
        }
    }
}
