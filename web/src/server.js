const express = require("express");
const multer = require("multer");
const cors = require("cors");
const { exec } = require("child_process");
const fs = require("fs");
const path = require("path");

const app = express();
const PORT = process.env.PORT || 5000;

app.use(cors());
app.use(express.json());

const uploadDir = path.join(__dirname, "uploads");
if (!fs.existsSync(uploadDir)) {
    fs.mkdirSync(uploadDir);
}

const storage = multer.diskStorage({
    destination: (req, file, cb) => {
        cb(null, uploadDir);
    },
    filename: (req, file, cb) => {
        cb(null, file.originalname);
    },
});

const cleanUploadsDir = () => {
    fs.readdir(uploadDir, (err, files) => {
        if (err) {
            console.error("Erreur lors de la lecture du dossier uploads:", err);
            return;
        }
        for (const file of files) {
            const filePath = path.join(uploadDir, file);
            fs.unlink(filePath, (err) => {
                if (err) {
                    console.error(
                        `Erreur lors de la suppression du fichier ${file}:`,
                        err
                    );
                } else {
                    console.log(`Fichier supprimé: ${file}`);
                }
            });
        }
    });
};

app.delete("/api/clear-uploads", (req, res) => {
    cleanUploadsDir();
    res.status(200).json({ message: "Dossier uploads nettoyé" });
});

const upload = multer({ storage });

const outputDir = path.join(__dirname, "compressed");

app.post("/api/compress", upload.single("image"), (req, res) => {
    const { nbClusters, convLimit } = req.body;
    const imagePath = path.join(uploadDir, req.file.originalname);

    const command = `python3 ./compressor.py ${imagePath} ${nbClusters} ${convLimit}`;

    exec(command, (error, stdout, stderr) => {
        if (error) {
            console.error(`Erreur: ${error.message}`);
            return res.status(500).json({
                message: "Erreur lors de la compression de l'image.",
                error: error.message,
            });
        }
        if (stderr) {
            console.error(`Erreur de stderr: ${stderr}`);
            return res
                .status(500)
                .json({ message: "Erreur de compression.", stderr: stderr });
        }

        const compressedImageName = `compressed_${req.file.originalname}`;
        const compressedImagePath = path.join(outputDir, compressedImageName);
        console.log("Image compressée:", compressedImageName);

        const compressedImageUrl = `http://localhost:${PORT}/uploads/compressed_${req.file.originalname}`;
        console.log("Image compressée:", compressedImageUrl);
        res.json({ imageUrl: compressedImageUrl });
    });
});
app.use("/uploads", express.static(path.join(__dirname, "uploads")));
app.listen(PORT, () => {
    console.log(`Server running on http://localhost:${PORT}`);
});
