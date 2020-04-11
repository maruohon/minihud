package fi.dy.masa.minihud.network;

import java.util.List;
import com.google.common.collect.ImmutableList;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import fi.dy.masa.malilib.network.IPluginChannelHandler;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.minihud.util.DataStorage;

public class StructurePacketHandler implements IPluginChannelHandler
{
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(new ResourceLocation("carpet:structures"));
    public static final int PACKET_S2C_DATA = 0;
    public static final int VERSION = 1;

    public static final StructurePacketHandler INSTANCE = new StructurePacketHandler();

    private boolean registered;
    private boolean valid;
    private int timeout;

    public void reset()
    {
        this.registered = false;
        this.valid = false;
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        int id = buf.readVarInt();

        if (id == PACKET_S2C_DATA)
        {
            CompoundNBT tag = buf.readCompoundTag();

            if (tag != null)
            {
                // Normal structure data packet
                if (this.registered && this.valid && tag.contains("Structures", Constants.NBT.TAG_LIST))
                {
                    ListNBT structures = tag.getList("Structures", Constants.NBT.TAG_COMPOUND);
                    DataStorage.getInstance().addOrUpdateStructuresFromServer(structures, this.timeout);
                }
                // Metadata packet upon channel registration
                else if (tag.contains("Version", Constants.NBT.TAG_INT) &&
                         tag.contains("Timeout", Constants.NBT.TAG_INT))
                {
                    this.registered = true;

                    if (tag.getInt("Version") == VERSION)
                    {
                        this.valid = true;
                        this.timeout = tag.getInt("Timeout");
                    }
                }
            }
        }
    }
}
