package fi.dy.masa.minihud.data.structure;

import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;

import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;

import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.data.structure.StructureDataUtils.StructureFileUtils;

public class StructureStorage
{
    public static final StructureStorage INSTANCE = new StructureStorage();

    private ArrayListMultimap<StructureType, StructureData> structureMap = ArrayListMultimap.create();
    @Nullable private BlockPos lastStructureUpdatePos;
    private boolean hasStructureDataFromServer;
    private boolean structuresDirty;
    private boolean structuresNeedUpdating;

    public void clear()
    {
        this.structuresNeedUpdating = true;
        this.hasStructureDataFromServer = false;
        this.structuresDirty = false;

        this.lastStructureUpdatePos = null;
        this.structureMap.clear();
    }

    public boolean hasStructureDataChanged()
    {
        return this.structuresDirty;
    }

    public void setStructuresNeedUpdating()
    {
        this.structuresNeedUpdating = true;
    }

    /**
     * Gets the structure data map, and clears the dirty flag
     */
    public ArrayListMultimap<StructureType, StructureData> getStructureDataAndClearDirtyFlag()
    {
        synchronized (INSTANCE)
        {
            this.structuresDirty = false;
            return this.structureMap;
        }
    }

    public void addStructureDataFromServer(ArrayListMultimap<StructureType, StructureData> data)
    {
        this.addStructureData(data);
        this.hasStructureDataFromServer = true;
    }

    public void addStructureDataFromLocalStructureFiles(ArrayListMultimap<StructureType, StructureData> data)
    {
        this.addStructureData(data);
        MiniHUD.debugLog("Structure data updated from local structure files, structure count = {}", data.size());
    }

    public void addStructureDataFromIntegratedServer(ArrayListMultimap<StructureType, StructureData> data)
    {
        this.addStructureData(data);
        MiniHUD.debugLog("Structure data updated from the integrated server, structure count = {}", data.size());
    }

    protected void addStructureData(ArrayListMultimap<StructureType, StructureData> data)
    {
        synchronized (INSTANCE)
        {
            this.structureMap = data;
            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            Entity player = GameUtils.getClientPlayer();

            if (player != null)
            {
                this.lastStructureUpdatePos = EntityWrap.getEntityBlockPos(player);
            }
        }
    }

    public void updateStructureDataIfNeeded()
    {
        if (GameUtils.getClientPlayer() != null)
        {
            BlockPos playerPos = EntityWrap.getPlayerBlockPos();

            if (GameUtils.isSinglePlayer())
            {
                if (this.structuresNeedUpdating(playerPos, 32))
                {
                    StructureDataUtils.IntegratedServer.updateStructureDataFromIntegratedServer(playerPos);
                }
            }
            else if (this.structuresNeedUpdating(playerPos, 128))
            {
                if (this.hasStructureDataFromServer)
                {
                    StructureDataUtils.requestStructureDataUpdates();
                }
                else
                {
                    this.addStructureDataFromLocalStructureFiles(StructureFileUtils.getEnabledStructuresFromNbtFiles());
                }
            }
        }
    }

    private boolean structuresNeedUpdating(BlockPos playerPos, int hysteresis)
    {
        return this.structuresNeedUpdating || this.lastStructureUpdatePos == null ||
                Math.abs(playerPos.getX() - this.lastStructureUpdatePos.getX()) >= hysteresis ||
                Math.abs(playerPos.getY() - this.lastStructureUpdatePos.getY()) >= hysteresis ||
                Math.abs(playerPos.getZ() - this.lastStructureUpdatePos.getZ()) >= hysteresis;
    }
}
