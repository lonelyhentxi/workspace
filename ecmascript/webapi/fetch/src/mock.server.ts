import express from 'express';
import bodyParser from 'body-parser';

const app = express();

app.use(bodyParser.urlencoded({extended: true}));
app.use(bodyParser.json());

app.post("/test", async (req, res) => {
    res.json({
        'haha': 'ac'
    });
});

app.listen(5001, () => {
    console.log('listening...');
});
