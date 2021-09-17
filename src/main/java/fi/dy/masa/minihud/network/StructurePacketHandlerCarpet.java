package fi.dy.masa.minihud.network;

import java.util.List;
import com.google.common.collect.ImmutableList;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.util.Identifier;
import fi.dy.masa.malilib.network.IPluginChannelHandler;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.util.DataStorage;

public class StructurePacketHandlerCarpet implements IPluginChannelHandler
{
    public static final List<Identifier> CHANNELS = ImmutableList.of(new Identifier("carpet:structures"));
    public static final int PACKET_S2C_DATA = 0;
    public static final int VERSION = 1;

    public static final StructurePacketHandlerCarpet INSTANCE = new StructurePacketHandlerCarpet();

    private boolean registered;
    private int timeout;

    public void reset()
    {
        this.registered = false;
    }

    @Override
    public List<Identifier> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketByteBuf buf)
    {
        int id = buf.readVarInt();

        MiniHUD.printDebug("StructurePacketHandlerCarpet#onPacketReceived(): " + id);

        if (id == PACKET_S2C_DATA)
        {
            NbtCompound tag = buf.readNbt();

            if (tag != null)
            {
                // Normal structure data packet
                if (this.registered && tag.contains("Structures", Constants.NBT.TAG_LIST))
                {
                    NbtList structures = tag.getList("Structures", Constants.NBT.TAG_COMPOUND);
                    DataStorage.getInstance().addOrUpdateStructuresFromServer(structures, this.timeout, false);
                }
                // Metadata packet upon channel registration
                else if (tag.contains("Version", Constants.NBT.TAG_INT) &&
                         tag.contains("Timeout", Constants.NBT.TAG_INT) &&
                         tag.getInt("Version") == VERSION)
                {
                    this.timeout = tag.getInt("Timeout");
                    this.registered = true;
                }
            }
        }
    }
}
