#include "opencv2/imgproc.hpp"
#include "opencv2/highgui.hpp"
#include<vector>

using namespace cv;
using namespace std;

void inline removeContours(vector<vector<Point>>& contours, int cmax, int cmin = 1)
{
	auto itc = contours.begin();
	while (itc != contours.end())
	{
		if ((itc->size()) < cmin || (itc->size()) > cmax)
		{
			itc = contours.erase(itc);
		}
		else ++itc;
	}
}


void dip1Main() {
	const auto raw_image = imread("../resources/3.bmp", IMREAD_GRAYSCALE);

	Mat denoised_image, bin_image;
	medianBlur(raw_image, denoised_image, 3);
	threshold(denoised_image, bin_image, 65, 255, cv::THRESH_BINARY);
	vector<vector<Point>> contours;
	findContours(bin_image, contours, cv::RETR_CCOMP, cv::CHAIN_APPROX_NONE, Point(0, 0));
	removeContours(contours, 20);
	auto mask_image = bin_image.clone();
	drawContours(mask_image, contours, -1, Scalar(0), cv::FILLED);
	findContours(mask_image, contours, cv::RETR_CCOMP, cv::CHAIN_APPROX_NONE, Point(0, 0));
	removeContours(contours, 30);
	drawContours(mask_image, contours, -1, Scalar(255), cv::FILLED);
	
	imshow("原图", raw_image);
	imshow("中值滤波", denoised_image);
	imshow("二值化", bin_image);
	imshow("形态学处理", mask_image);
	imwrite("../results/denoised.bmp", denoised_image);
	imwrite("../results/bin.bmp", bin_image);
	imwrite("../results/mask.bmp", mask_image);

	waitKey();
	destroyAllWindows();
}