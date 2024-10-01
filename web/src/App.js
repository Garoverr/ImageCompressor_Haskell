import React, { useState } from "react";

function App() {
    const [file, setFile] = useState(null);
    const [nbClusters, setNbClusters] = useState(5);
    const [convLimit, setConvLimit] = useState(0.01);
    const [compressedImageUrl, setCompressedImageUrl] = useState("");

    const handleFileChange = (event) => {
        setFile(event.target.files[0]);
    };

    const handleCompress = async () => {
        const formData = new FormData();
        formData.append("image", file);
        formData.append("nbClusters", nbClusters);
        formData.append("convLimit", convLimit);

        try {
            const response = await fetch("http://localhost:5000/api/compress", {
                method: "POST",
                body: formData,
            });

            if (!response.ok) {
                throw new Error("Erreur lors de la compression");
            }

            const data = await response.json();
            console.log("Image compressée:", data.imageUrl);
            console.log("data", data);
            setCompressedImageUrl(data.imageUrl);
        } catch (error) {
            console.error("Erreur:", error);
        }
    };

    const handleClearStorage = async () => {
      try {
        const response = await fetch('http://localhost:5000/api/clear-uploads', {
          method: 'DELETE',
        });
  
        if (!response.ok) {
          throw new Error('Erreur lors du nettoyage');
        }
  
        console.log('Dossier uploads nettoyé');
        setCompressedImageUrl('');
      } catch (error) {
        console.error('Erreur:', error);
      }
    };

    return (
        <div>
            <h1>Compression d'image</h1>
            <input type="file" onChange={handleFileChange} />
            <div>
                <label>Nombre de clusters: </label>
                <input
                    type="number"
                    value={nbClusters}
                    onChange={(e) => setNbClusters(e.target.value)}
                />
            </div>
            <div>
                <label>Limite de convergence: </label>
                <input
                    type="number"
                    step="0.01"
                    value={convLimit}
                    onChange={(e) => setConvLimit(e.target.value)}
                />
            </div>
            <button onClick={handleCompress}>Compresser</button>
            <button onClick={handleClearStorage}>Clear</button>

            {compressedImageUrl && (
                <div>
                    <h2>Image Compressée :</h2>
                    <img src={compressedImageUrl} alt="Compressed" />
                </div>
            )}
        </div>
    );
}

export default App;
